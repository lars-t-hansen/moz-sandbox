(module
  (type $FUNCSIG$vii (func (param i32 i32)))
  (type $FUNCSIG$id (func (param f64) (result i32)))
  (type $FUNCSIG$iiiiddi (func (param i32 i32 i32 f64 f64 i32) (result i32)))
  (type $FUNCSIG$viii (func (param i32 i32 i32)))
  (type $FUNCSIG$iii (func (param i32 i32) (result i32)))
  (type $FUNCSIG$vi (func (param i32)))
  (type $FUNCSIG$iiii (func (param i32 i32 i32) (result i32)))
  (type $FUNCSIG$v (func))
  (type $FUNCSIG$viiii (func (param i32 i32 i32 i32)))
  (type $FUNCSIG$viiiiii (func (param i32 i32 i32 i32 i32 i32)))
  (type $FUNCSIG$viiiii (func (param i32 i32 i32 i32 i32)))
  (type $FUNCSIG$ii (func (param i32) (result i32)))
  (type $FUNCSIG$i (func (result i32)))
  (import "env" "DYNAMICTOP_PTR" (global $DYNAMICTOP_PTR$asm2wasm$import i32))
  (import "env" "STACKTOP" (global $STACKTOP$asm2wasm$import i32))
  (import "env" "STACK_MAX" (global $STACK_MAX$asm2wasm$import i32))
  (import "env" "abort" (func $abort (param i32)))
  (import "env" "enlargeMemory" (func $enlargeMemory (result i32)))
  (import "env" "getTotalMemory" (func $getTotalMemory (result i32)))
  (import "env" "abortOnCannotGrowMemory" (func $abortOnCannotGrowMemory (result i32)))
  (import "env" "_pthread_cleanup_pop" (func $_pthread_cleanup_pop (param i32)))
  (import "env" "___lock" (func $___lock (param i32)))
  (import "env" "___syscall146" (func $___syscall146 (param i32 i32) (result i32)))
  (import "env" "___cxa_throw" (func $___cxa_throw (param i32 i32 i32)))
  (import "env" "___syscall54" (func $___syscall54 (param i32 i32) (result i32)))
  (import "env" "___syscall6" (func $___syscall6 (param i32 i32) (result i32)))
  (import "env" "___setErrNo" (func $___setErrNo (param i32)))
  (import "env" "_abort" (func $_abort))
  (import "env" "___syscall140" (func $___syscall140 (param i32 i32) (result i32)))
  (import "env" "_gettimeofday" (func $_gettimeofday (param i32 i32) (result i32)))
  (import "env" "_pthread_cleanup_push" (func $_pthread_cleanup_push (param i32 i32)))
  (import "env" "_emscripten_memcpy_big" (func $_emscripten_memcpy_big (param i32 i32 i32) (result i32)))
  (import "env" "___unlock" (func $___unlock (param i32)))
  (import "env" "_exit" (func $_exit (param i32)))
  (import "env" "___assert_fail" (func $___assert_fail (param i32 i32 i32 i32)))
  (import "env" "___cxa_allocate_exception" (func $___cxa_allocate_exception (param i32) (result i32)))
  (import "asm2wasm" "f64-to-int" (func $f64-to-int (param f64) (result i32)))
  (import "asm2wasm" "i32u-rem" (func $i32u-rem (param i32 i32) (result i32)))
  (import "asm2wasm" "i32s-div" (func $i32s-div (param i32 i32) (result i32)))
  (import "asm2wasm" "i32s-rem" (func $i32s-rem (param i32 i32) (result i32)))
  (import "asm2wasm" "i32u-div" (func $i32u-div (param i32 i32) (result i32)))
  (import "env" "memory" (memory $0 256 256))
  (import "env" "table" (table 81 81 anyfunc))
  (import "env" "memoryBase" (global $memoryBase i32))
  (import "env" "tableBase" (global $tableBase i32))
  (global $DYNAMICTOP_PTR (mut i32) (get_global $DYNAMICTOP_PTR$asm2wasm$import))
  (global $STACKTOP (mut i32) (get_global $STACKTOP$asm2wasm$import))
  (global $STACK_MAX (mut i32) (get_global $STACK_MAX$asm2wasm$import))
  (global $__THREW__ (mut i32) (i32.const 0))
  (global $threwValue (mut i32) (i32.const 0))
  (global $tempRet0 (mut i32) (i32.const 0))
  (elem (get_global $tableBase) $b0 $___stdio_write $___stdio_seek $___stdout_write $_sn_write $__ZNK10__cxxabiv117__class_type_info9can_catchEPKNS_16__shim_type_infoERPv $b0 $b0 $b1 $__ZNK10__cxxabiv117__class_type_info16search_below_dstEPNS_19__dynamic_cast_infoEPKvib $__ZNK10__cxxabiv120__si_class_type_info16search_below_dstEPNS_19__dynamic_cast_infoEPKvib $b1 $b2 $___unlockfile $__ZN10__cxxabiv117__class_type_infoD0Ev $___unlockfile $___unlockfile $__ZN10__cxxabiv117__class_type_infoD0Ev $___unlockfile $__ZN10__cxxabiv117__class_type_infoD0Ev $_cleanup_387 $b2 $b2 $b2 $b2 $b2 $b2 $b2 $b3 $__ZN8Triangle9intersectERK4Vec3S2_ddPd $__ZN6Jumble9intersectERK4Vec3S2_ddPd $__ZN6Volume9intersectERK4Vec3S2_ddPd $__ZN6Sphere9intersectERK4Vec3S2_ddPd $b3 $b3 $b3 $b4 $__ZN8Triangle6boundsEv $__ZN8Triangle6centerEv $__ZN6Jumble6boundsEv $__ZN6Jumble6centerEv $__ZN6Volume6boundsEv $__ZN6Volume6centerEv $__ZN6Sphere6boundsEv $__ZN6Sphere6centerEv $b4 $b4 $b4 $b4 $b4 $b4 $b4 $b5 $___stdio_close $__ZNKSt9bad_alloc4whatEv $b5 $b6 $__ZN8Triangle6normalERK4Vec3 $__ZN8Triangle5debugEPFvPKcEj $__ZN6Jumble6normalERK4Vec3 $__ZN6Jumble5debugEPFvPKcEj $__ZN6Volume6normalERK4Vec3 $__ZN6Volume5debugEPFvPKcEj $__ZN6Sphere6normalERK4Vec3 $__ZN6Sphere5debugEPFvPKcEj $b6 $b6 $b6 $b6 $b6 $b6 $b6 $b7 $b8 $__ZNK10__cxxabiv117__class_type_info16search_above_dstEPNS_19__dynamic_cast_infoEPKvS4_ib $__ZNK10__cxxabiv120__si_class_type_info16search_above_dstEPNS_19__dynamic_cast_infoEPKvS4_ib $b8 $b9 $__ZNK10__cxxabiv117__class_type_info27has_unambiguous_public_baseEPNS_19__dynamic_cast_infoEPvi $__ZNK10__cxxabiv120__si_class_type_info27has_unambiguous_public_baseEPNS_19__dynamic_cast_infoEPvi $b9)
  (data (i32.const 1024) "\aeG\e1z\14\ae\df?\e1z\14\aeG\e1\ea?\'1\08\ac\1cZ\e4??5^\baI\0c\da?9\b4\c8v\be\9f\9a?\1b/\dd$\06\81\e1?%\06\81\95C\8b\cc?\bct\93\18\04V\d6?L7\89A`\e5\a0?\fa~j\bct\93\e4?B`\e5\d0\"\db\e1?b\10X9\b4\c8\de?+\87\16\d9\ce\f7\a3?ffffff\c6?o\12\83\c0\ca\a1\c5?1\08\ac\1cZd\e3?\d1\"\db\f9~j\e4?\b2\9d\ef\a7\c6K\d7?\e3\a5\9b\c4 \b0\e6??5^\baI\0c\ea?\91\ed|?5^\ca?\be\9f\1a/\dd$\d6?\96C\8bl\e7\fb\e9?\b2\9d\ef\a7\c6K\e3?5^\baI\0c\02\ef?\d1\"\db\f9~j\ec?7\89A`\e5\d0\d2?\91\ed|?5^\ea?=\n\d7\a3p=\da?\17\d9\ce\f7S\e3\dd?\19\04V\0e-\b2\9d?d;\dfO\8d\97\e6?\7fj\bct\93\18\d4?\c8\08\00\00\81\t\00\00\f0\08\00\00w\t\00\00\08\05\00\00\00\00\00\00\f0\08\00\00\af\t\00\00\08\05\00\00\00\00\00\00\f0\08\00\00\1d\n\00\00\08\05\00\00\00\00\00\00\f0\08\00\00r\n\00\00\08\05")
  (data (i32.const 1363) "@\fb!\f9?\00\00\00\00-Dt>\00\00\00\80\98F\f8<\00\00\00`Q\ccx;\00\00\00\80\83\1b\f09\00\00\00@ %z8\00\00\00\80\"\82\e36\00\00\00\00\1d\f3i5\c8\08\00\00\89\14\00\00\f0\08\00\00\e9\14\00\00\a8\05\00\00\00\00\00\00\f0\08\00\00\96\14\00\00\b8\05\00\00\00\00\00\00\c8\08\00\00\b7\14\00\00\f0\08\00\00\c4\14\00\00\98\05\00\00\00\00\00\00\f0\08\00\00\1a\15\00\00\90\05")
  (data (i32.const 1508) "\10\05\00\00\01\00\00\00\01\00\00\00\01\00\00\00\02\00\00\00\02\00\00\00\00\00\00\00 \05\00\00\02\00\00\00\03\00\00\00\03\00\00\00\04\00\00\00\04\00\00\00\00\00\00\000\05\00\00\03\00\00\00\05\00\00\00\05\00\00\00\06\00\00\00\06\00\00\00\00\00\00\00@\05\00\00\04\00\00\00\07\00\00\00\07\00\00\00\08\00\00\00\08\00\00\00T\06\00\00\05")
  (data (i32.const 1632) "\01")
  (data (i32.const 1656) "\01\00\00\00\02\00\00\00\a4\17")
  (data (i32.const 1680) "\02")
  (data (i32.const 1695) "\ff\ff\ff\ff\ff")
  (data (i32.const 1732) "\05")
  (data (i32.const 1744) "\01")
  (data (i32.const 1768) "\03\00\00\00\02\00\00\00\ac\17\00\00\00\04")
  (data (i32.const 1792) "\01")
  (data (i32.const 1807) "\n\ff\ff\ff\ff")
  (data (i32.const 1844) "\c4\06")
  (data (i32.const 1884) "\04")
  (data (i32.const 1923) "\ff\ff\ff\ff\ff")
  (data (i32.const 1960) "\03\00\00\00\04\00\00\00\04\00\00\00\06\00\00\00\83\f9\a2\00DNn\00\fc)\15\00\d1W\'\00\dd4\f5\00b\db\c0\00<\99\95\00A\90C\00cQ\fe\00\bb\de\ab\00\b7a\c5\00:n$\00\d2MB\00I\06\e0\00\t\ea.\00\1c\92\d1\00\eb\1d\fe\00)\b1\1c\00\e8>\a7\00\f55\82\00D\bb.\00\9c\e9\84\00\b4&p\00A~_\00\d6\919\00S\839\00\9c\f49\00\8b_\84\00(\f9\bd\00\f8\1f;\00\de\ff\97\00\0f\98\05\00\11/\ef\00\nZ\8b\00m\1fm\00\cf~6\00\t\cb\'\00FO\b7\00\9ef?\00-\ea_\00\ba\'u\00\e5\eb\c7\00={\f1\00\f79\07\00\92R\8a\00\fbk\ea\00\1f\b1_\00\08]\8d\000\03V\00{\fcF\00\f0\abk\00 \bc\cf\006\f4\9a\00\e3\a9\1d\00^a\91\00\08\1b\e6\00\85\99e\00\a0\14_\00\8d@h\00\80\d8\ff\00\'sM\00\06\061\00\caV\15\00\c9\a8s\00{\e2`\00k\8c\c0\00\00\00\00\00\98\05\00\00\01\00\00\00\02\00\00\00\03\00\00\00\04\00\00\00\05\00\00\00\01\00\00\00\01\00\00\00\01\00\00\00\00\00\00\00\c0\05\00\00\01\00\00\00\05\00\00\00\03\00\00\00\04\00\00\00\05\00\00\00\02\00\00\00\02\00\00\00\02\00\00\00\00\00\00\00\d0\05\00\00\06\00\00\00\07\00\00\00\02\00\00\00CRASH: %s\n\00WARNING: %s\n\00Setup time: %g ms\n\00Render time: %g ms\n\00Degenerate parition\008Triangle\007Surface\00[T (%g,%g,%g) (%g,%g,%g) (%g,%g,%g)]\006Jumble\00Normal not implemented for Jumble\00Bounds not implemented for Jumble\00Center not implemented for Jumble\006Volume\00Normal not implemented for Volume\00Center not implemented for Volume\00[\00,\n\00 \00]\006Sphere\00(S c=(%g,%g,%g) r=%g)\00\11\00\n\00\11\11\11\00\00\00\00\05\00\00\00\00\00\00\t\00\00\00\00\0b")
  (data (i32.const 2736) "\11\00\0f\n\11\11\11\03\n\07\00\01\13\t\0b\0b\00\00\t\06\0b\00\00\0b\00\06\11\00\00\00\11\11\11")
  (data (i32.const 2785) "\0b")
  (data (i32.const 2794) "\11\00\n\n\11\11\11\00\n\00\00\02\00\t\0b\00\00\00\t\00\0b\00\00\0b")
  (data (i32.const 2843) "\0c")
  (data (i32.const 2855) "\0c\00\00\00\00\0c\00\00\00\00\t\0c\00\00\00\00\00\0c\00\00\0c")
  (data (i32.const 2901) "\0e")
  (data (i32.const 2913) "\0d\00\00\00\04\0d\00\00\00\00\t\0e\00\00\00\00\00\0e\00\00\0e")
  (data (i32.const 2959) "\10")
  (data (i32.const 2971) "\0f\00\00\00\00\0f\00\00\00\00\t\10\00\00\00\00\00\10\00\00\10\00\00\12\00\00\00\12\12\12")
  (data (i32.const 3026) "\12\00\00\00\12\12\12\00\00\00\00\00\00\t")
  (data (i32.const 3075) "\0b")
  (data (i32.const 3087) "\n\00\00\00\00\n\00\00\00\00\t\0b\00\00\00\00\00\0b\00\00\0b")
  (data (i32.const 3133) "\0c")
  (data (i32.const 3145) "\0c\00\00\00\00\0c\00\00\00\00\t\0c\00\00\00\00\00\0c\00\00\0c\00\000123456789ABCDEF-+   0X0x\00(null)\00-0X+0X 0X-0x+0x 0x\00inf\00INF\00nan\00NAN\00.\00T!\"\19\0d\01\02\03\11K\1c\0c\10\04\0b\1d\12\1e\'hnopqb \05\06\0f\13\14\15\1a\08\16\07($\17\18\t\n\0e\1b\1f%#\83\82}&*+<=>?CGJMXYZ[\\]^_`acdefgijklrstyz{|\00Illegal byte sequence\00Domain error\00Result not representable\00Not a tty\00Permission denied\00Operation not permitted\00No such file or directory\00No such process\00File exists\00Value too large for data type\00No space left on device\00Out of memory\00Resource busy\00Interrupted system call\00Resource temporarily unavailable\00Invalid seek\00Cross-device link\00Read-only file system\00Directory not empty\00Connection reset by peer\00Operation timed out\00Connection refused\00Host is down\00Host is unreachable\00Address in use\00Broken pipe\00I/O error\00No such device or address\00Block device required\00No such device\00Not a directory\00Is a directory\00Text file busy\00Exec format error\00Invalid argument\00Argument list too long\00Symbolic link loop\00Filename too long\00Too many open files in system\00No file descriptors available\00Bad file descriptor\00No child process\00Bad address\00File too large\00Too many links\00No locks available\00Resource deadlock would occur\00State not recoverable\00Previous owner died\00Operation canceled\00Function not implemented\00No message of desired type\00Identifier removed\00Device not a stream\00No data available\00Device timeout\00Out of streams resources\00Link has been severed\00Protocol error\00Bad message\00File descriptor in bad state\00Not a socket\00Destination address required\00Message too large\00Protocol wrong type for socket\00Protocol not available\00Protocol not supported\00Socket type not supported\00Not supported\00Protocol family not supported\00Address family not supported by protocol\00Address not available\00Network is down\00Network unreachable\00Connection reset by network\00Connection aborted\00No buffer space available\00Socket is connected\00Socket not connected\00Cannot send after socket shutdown\00Operation already in progress\00Operation in progress\00Stale file handle\00Remote I/O error\00Quota exceeded\00No medium found\00Wrong medium type\00No error information\00\00!\"vector length_error\"\00/Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector\00__throw_length_error\00St9exception\00N10__cxxabiv116__shim_type_infoE\00St9type_info\00N10__cxxabiv120__si_class_type_infoE\00N10__cxxabiv117__class_type_infoE\00std::bad_alloc\00St9bad_alloc")
  (export "_sbrk" (func $_sbrk))
  (export "_free" (func $_free))
  (export "_main" (func $_main))
  (export "_memmove" (func $_memmove))
  (export "_pthread_self" (func $_pthread_self))
  (export "_memset" (func $_memset))
  (export "_malloc" (func $_malloc))
  (export "_memcpy" (func $_memcpy))
  (export "_fflush" (func $_fflush))
  (export "___errno_location" (func $___errno_location))
  (export "runPostSets" (func $runPostSets))
  (export "stackAlloc" (func $stackAlloc))
  (export "stackSave" (func $stackSave))
  (export "stackRestore" (func $stackRestore))
  (export "establishStackSpace" (func $establishStackSpace))
  (export "setThrew" (func $setThrew))
  (export "setTempRet0" (func $setTempRet0))
  (export "getTempRet0" (func $getTempRet0))
  (export "dynCall_iiii" (func $dynCall_iiii))
  (export "dynCall_viiiii" (func $dynCall_viiiii))
  (export "dynCall_vi" (func $dynCall_vi))
  (export "dynCall_iiiiddi" (func $dynCall_iiiiddi))
  (export "dynCall_vii" (func $dynCall_vii))
  (export "dynCall_ii" (func $dynCall_ii))
  (export "dynCall_viii" (func $dynCall_viii))
  (export "dynCall_v" (func $dynCall_v))
  (export "dynCall_viiiiii" (func $dynCall_viiiiii))
  (export "dynCall_viiii" (func $dynCall_viiii))
  (func $stackAlloc (param $0 i32) (result i32)
    (local $1 i32)
    (set_local $1
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (get_local $0)
      )
    )
    (set_global $STACKTOP
      (i32.and
        (i32.add
          (get_global $STACKTOP)
          (i32.const 15)
        )
        (i32.const -16)
      )
    )
    (get_local $1)
  )
  (func $stackSave (result i32)
    (get_global $STACKTOP)
  )
  (func $stackRestore (param $0 i32)
    (set_global $STACKTOP
      (get_local $0)
    )
  )
  (func $establishStackSpace (param $0 i32) (param $1 i32)
    (set_global $STACKTOP
      (get_local $0)
    )
    (set_global $STACK_MAX
      (get_local $1)
    )
  )
  (func $setThrew (param $0 i32) (param $1 i32)
    (if
      (i32.eqz
        (get_global $__THREW__)
      )
      (block
        (set_global $__THREW__
          (get_local $0)
        )
        (set_global $threwValue
          (get_local $1)
        )
      )
    )
  )
  (func $setTempRet0 (param $0 i32)
    (set_global $tempRet0
      (get_local $0)
    )
  )
  (func $getTempRet0 (result i32)
    (get_global $tempRet0)
  )
  (func $__Z5CRASHPKc (param $0 i32)
    (local $1 i32)
    (local $2 i32)
    (set_local $1
      (get_global $STACKTOP)
    )
    ;; raybench.cpp:120
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 16)
      )
    )
    ;; raybench.cpp:121
    (set_local $2
      (i32.load
        (i32.const 1616)
      )
    )
    ;; raybench.cpp:121
    (i32.store
      (get_local $1)
      (get_local $0)
    )
    ;; raybench.cpp:121
    (drop
      (call $_fprintf
        (get_local $2)
        (i32.const 2340)
        (get_local $1)
      )
    )
    ;; raybench.cpp:122
    (call $_exit
      (i32.const 1)
    )
  )
  (func $_main (param $0 i32) (param $1 i32) (result i32)
    (local $2 i32)
    (local $3 i32)
    (local $4 i32)
    (local $5 i32)
    (local $6 i32)
    (local $7 i32)
    (local $8 i32)
    (local $9 i32)
    (local $10 i32)
    (local $11 i32)
    (local $12 i32)
    (set_local $1
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 112)
      )
    )
    (set_local $8
      (i32.add
        (get_local $1)
        (i32.const 80)
      )
    )
    (i64.store
      (tee_local $5
        (i32.add
          (get_local $1)
          (i32.const 48)
        )
      )
      (i64.const 0)
    )
    (i64.store offset=8
      (get_local $5)
      (i64.const 0)
    )
    ;; raybench.cpp:145
    (i64.store offset=16
      (get_local $5)
      (i64.const 0)
    )
    (i64.store
      (tee_local $6
        (i32.add
          (get_local $1)
          (i32.const 24)
        )
      )
      (i64.const 0)
    )
    (i64.store offset=8
      (get_local $6)
      (i64.const 0)
    )
    ;; raybench.cpp:145
    (i64.store offset=16
      (get_local $6)
      (i64.const 0)
    )
    (i64.store
      (tee_local $3
        (get_local $1)
      )
      (i64.const 0)
    )
    (i64.store offset=8
      (get_local $3)
      (i64.const 0)
    )
    ;; raybench.cpp:145
    (i64.store offset=16
      (get_local $3)
      (i64.const 0)
    )
    ;; raybench.cpp:136
    (drop
      (call $_gettimeofday
        (tee_local $2
          (i32.add
            (get_local $1)
            (i32.const 88)
          )
        )
        (i32.const 0)
      )
    )
    ;; raybench.cpp:137
    (set_local $0
      (i32.load
        (get_local $2)
      )
    )
    ;; raybench.cpp:137
    (set_local $7
      (i32.load offset=4
        (get_local $2)
      )
    )
    ;; raybench.cpp:634
    (set_local $10
      (call $__Z8setStageP4Vec3S0_S0_
        (get_local $5)
        (get_local $6)
        (get_local $3)
      )
    )
    ;; raybench.cpp:136
    (drop
      (call $_gettimeofday
        (get_local $2)
        (i32.const 0)
      )
    )
    ;; raybench.cpp:137
    (set_local $11
      (i32.load
        (get_local $2)
      )
    )
    ;; raybench.cpp:137
    (set_local $12
      (i32.load offset=4
        (get_local $2)
      )
    )
    ;; raybench.cpp:645
    (set_local $9
      (i32.load
        (i32.const 1616)
      )
    )
    ;; raybench.cpp:636
    (f64.store
      (tee_local $4
        (i32.add
          (get_local $1)
          (i32.const 72)
        )
      )
      (f64.div
        (f64.convert_u/i64
          (i64.add
            (i64.sub
              (i64.extend_s/i32
                (get_local $12)
              )
              (i64.extend_s/i32
                (get_local $7)
              )
            )
            (i64.mul
              (i64.sub
                (i64.extend_s/i32
                  (get_local $11)
                )
                (i64.extend_s/i32
                  (get_local $0)
                )
              )
              (i64.const 1000000)
            )
          )
        )
        (f64.const 1e3)
      )
    )
    ;; raybench.cpp:636
    (drop
      (call $_fprintf
        (get_local $9)
        (i32.const 2364)
        (get_local $4)
      )
    )
    ;; raybench.cpp:595
    (i32.store
      (get_local $2)
      (i32.const 600)
    )
    ;; raybench.cpp:596
    (i32.store offset=4
      (get_local $2)
      (i32.const 800)
    )
    ;; raybench.cpp:597
    (set_local $7
      (call $__Znaj
        (i32.const 1920000)
      )
    )
    ;; raybench.cpp:597
    (i32.store offset=8
      (get_local $2)
      (get_local $7)
    )
    ;; raybench.cpp:604
    (set_local $0
      (i32.const 0)
    )
    (loop $while-in
      ;; raybench.cpp:604
      (i32.store
        (i32.add
          (get_local $7)
          (i32.shl
            (get_local $0)
            (i32.const 2)
          )
        )
        (i32.const -6817129)
      )
      (br_if $while-in
        (i32.ne
          (tee_local $0
            (i32.add
              (get_local $0)
              (i32.const 1)
            )
          )
          (i32.const 480000)
        )
      )
    )
    ;; raybench.cpp:136
    (drop
      (call $_gettimeofday
        (get_local $4)
        (i32.const 0)
      )
    )
    ;; raybench.cpp:137
    (set_local $0
      (i32.load
        (get_local $4)
      )
    )
    ;; raybench.cpp:137
    (set_local $7
      (i32.load offset=4
        (get_local $4)
      )
    )
    (i64.store
      (i32.const 5416)
      (i64.load
        (get_local $5)
      )
    )
    (i64.store
      (i32.const 5424)
      (i64.load offset=8
        (get_local $5)
      )
    )
    ;; raybench.cpp:703
    (i64.store
      (i32.const 5432)
      (i64.load offset=16
        (get_local $5)
      )
    )
    (i64.store
      (i32.const 5464)
      (i64.load
        (get_local $6)
      )
    )
    (i64.store
      (i32.const 5472)
      (i64.load offset=8
        (get_local $6)
      )
    )
    ;; raybench.cpp:704
    (i64.store
      (i32.const 5480)
      (i64.load offset=16
        (get_local $6)
      )
    )
    (i64.store
      (i32.const 5440)
      (i64.load
        (get_local $3)
      )
    )
    (i64.store
      (i32.const 5448)
      (i64.load offset=8
        (get_local $3)
      )
    )
    ;; raybench.cpp:705
    (i64.store
      (i32.const 5456)
      (i64.load offset=16
        (get_local $3)
      )
    )
    ;; raybench.cpp:706
    (i32.store
      (i32.const 5488)
      (get_local $10)
    )
    ;; raybench.cpp:707
    (i32.store
      (i32.const 5492)
      (get_local $2)
    )
    ;; raybench.cpp:709
    (call $__Z18traceWithAntialiasjjjj
      (i32.const 0)
      (i32.const 600)
      (i32.const 0)
      (i32.const 800)
    )
    ;; raybench.cpp:136
    (drop
      (call $_gettimeofday
        (get_local $4)
        (i32.const 0)
      )
    )
    ;; raybench.cpp:137
    (set_local $3
      (i32.load
        (get_local $4)
      )
    )
    ;; raybench.cpp:137
    (set_local $2
      (i32.load offset=4
        (get_local $4)
      )
    )
    ;; raybench.cpp:645
    (f64.store
      (get_local $8)
      (f64.div
        (f64.convert_u/i64
          (i64.add
            (i64.sub
              (i64.extend_s/i32
                (get_local $2)
              )
              (i64.extend_s/i32
                (get_local $7)
              )
            )
            (i64.mul
              (i64.sub
                (i64.extend_s/i32
                  (get_local $3)
                )
                (i64.extend_s/i32
                  (get_local $0)
                )
              )
              (i64.const 1000000)
            )
          )
        )
        (f64.const 1e3)
      )
    )
    ;; raybench.cpp:645
    (drop
      (call $_fprintf
        (get_local $9)
        (i32.const 2383)
        (get_local $8)
      )
    )
    (set_global $STACKTOP
      (get_local $1)
    )
    (i32.const 0)
  )
  (func $__Z8setStageP4Vec3S0_S0_ (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
    (local $3 i32)
    (local $4 i32)
    (local $5 i32)
    (local $6 i32)
    (local $7 i32)
    (local $8 i32)
    (local $9 i32)
    (local $10 i32)
    (local $11 f64)
    (local $12 i32)
    (local $13 f64)
    (local $14 i32)
    (local $15 f64)
    (local $16 f64)
    (local $17 i32)
    (local $18 i32)
    (local $19 i32)
    (local $20 f64)
    (local $21 f64)
    (local $22 f64)
    (set_local $12
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 464)
      )
    )
    ;; raybench.cpp:237
    (f64.store
      (tee_local $5
        (i32.add
          (get_local $12)
          (i32.const 304)
        )
      )
      (f64.const 0.26406250000000003)
    )
    ;; raybench.cpp:237
    (f64.store offset=8
      (get_local $5)
      (f64.const 0.26406250000000003)
    )
    ;; raybench.cpp:237
    (f64.store offset=16
      (get_local $5)
      (f64.const 0.26406250000000003)
    )
    ;; raybench.cpp:238
    (f64.store offset=24
      (get_local $5)
      (f64.const 0.19804687499999998)
    )
    ;; raybench.cpp:238
    (f64.store offset=32
      (get_local $5)
      (f64.const 0.19804687499999998)
    )
    ;; raybench.cpp:238
    (f64.store offset=40
      (get_local $5)
      (f64.const 0.19804687499999998)
    )
    ;; raybench.cpp:239
    (i32.store offset=48
      (get_local $5)
      (i32.const 100)
    )
    ;; raybench.cpp:240
    (f64.store offset=56
      (get_local $5)
      (f64.const 0.19804687499999998)
    )
    ;; raybench.cpp:240
    (f64.store offset=64
      (get_local $5)
      (f64.const 0.19804687499999998)
    )
    ;; raybench.cpp:240
    (f64.store offset=72
      (get_local $5)
      (f64.const 0.19804687499999998)
    )
    ;; raybench.cpp:241
    (f64.store offset=80
      (get_local $5)
      (f64.const 0.5)
    )
    ;; raybench.cpp:237
    (f64.store
      (tee_local $4
        (i32.add
          (get_local $12)
          (i32.const 216)
        )
      )
      (f64.const 0.23750000000000002)
    )
    ;; raybench.cpp:237
    (f64.store offset=8
      (get_local $4)
      (f64.const 0.3921875)
    )
    ;; raybench.cpp:237
    (f64.store offset=16
      (get_local $4)
      (f64.const 0.23750000000000002)
    )
    ;; raybench.cpp:238
    (f64.store offset=24
      (get_local $4)
      (f64.const 0.23750000000000002)
    )
    ;; raybench.cpp:238
    (f64.store offset=32
      (get_local $4)
      (f64.const 0.3921875)
    )
    ;; raybench.cpp:238
    (f64.store offset=40
      (get_local $4)
      (f64.const 0.23750000000000002)
    )
    ;; raybench.cpp:239
    (i32.store offset=48
      (get_local $4)
      (i32.const 10)
    )
    ;; raybench.cpp:240
    (f64.store offset=56
      (get_local $4)
      (f64.const 0.11875000000000001)
    )
    ;; raybench.cpp:240
    (f64.store offset=64
      (get_local $4)
      (f64.const 0.19609375)
    )
    ;; raybench.cpp:240
    (f64.store offset=72
      (get_local $4)
      (f64.const 0.11875000000000001)
    )
    ;; raybench.cpp:241
    (f64.store offset=80
      (get_local $4)
      (f64.const 1)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:432
    (i32.store
      (tee_local $9
        (i32.add
          (get_local $12)
          (i32.const 440)
        )
      )
      (i32.const 0)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:433
    (i32.store
      (tee_local $8
        (i32.add
          (get_local $9)
          (i32.const 4)
        )
      )
      (i32.const 0)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:2252
    (i32.store offset=8
      (get_local $9)
      (i32.const 0)
    )
    ;; raybench.cpp:926
    (set_local $3
      (call $__Znwj
        (i32.const 128)
      )
    )
    ;; raybench.cpp:277
    (f64.store offset=8
      (get_local $3)
      (f64.const 0.1)
    )
    ;; raybench.cpp:277
    (f64.store offset=16
      (get_local $3)
      (f64.const 0.2)
    )
    ;; raybench.cpp:277
    (f64.store offset=24
      (get_local $3)
      (f64.const 0.2)
    )
    ;; raybench.cpp:277
    (f64.store offset=32
      (get_local $3)
      (f64.const 0.3)
    )
    ;; raybench.cpp:277
    (f64.store offset=40
      (get_local $3)
      (f64.const 0.6)
    )
    ;; raybench.cpp:277
    (f64.store offset=48
      (get_local $3)
      (f64.const 0.6)
    )
    ;; raybench.cpp:277
    (i32.store offset=56
      (get_local $3)
      (i32.const 10)
    )
    ;; raybench.cpp:277
    (f64.store offset=64
      (get_local $3)
      (f64.const 0.05)
    )
    ;; raybench.cpp:277
    (f64.store offset=72
      (get_local $3)
      (f64.const 0.1)
    )
    ;; raybench.cpp:277
    (f64.store offset=80
      (get_local $3)
      (f64.const 0.1)
    )
    ;; raybench.cpp:277
    (f64.store offset=88
      (get_local $3)
      (f64.const 0)
    )
    ;; raybench.cpp:453
    (i32.store
      (get_local $3)
      (i32.const 1596)
    )
    ;; raybench.cpp:451
    (f64.store offset=96
      (get_local $3)
      (f64.const -1)
    )
    ;; raybench.cpp:451
    (f64.store offset=104
      (get_local $3)
      (f64.const 1)
    )
    ;; raybench.cpp:451
    (f64.store offset=112
      (get_local $3)
      (f64.const -9)
    )
    ;; raybench.cpp:452
    (f64.store offset=120
      (get_local $3)
      (f64.const 1)
    )
    ;; raybench.cpp:926
    (i32.store
      (tee_local $7
        (i32.add
          (get_local $12)
          (i32.const 392)
        )
      )
      (get_local $3)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1614
    (call $__ZNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEE21__push_back_slow_pathIS2_EEvOT_
      (get_local $9)
      (get_local $7)
    )
    ;; raybench.cpp:927
    (set_local $3
      (call $__Znwj
        (i32.const 128)
      )
    )
    ;; raybench.cpp:277
    (f64.store offset=8
      (get_local $3)
      (f64.const 0.3)
    )
    ;; raybench.cpp:277
    (f64.store offset=16
      (get_local $3)
      (f64.const 0.3)
    )
    ;; raybench.cpp:277
    (f64.store offset=24
      (get_local $3)
      (f64.const 0.2)
    )
    ;; raybench.cpp:277
    (f64.store offset=32
      (get_local $3)
      (f64.const 0.6)
    )
    ;; raybench.cpp:277
    (f64.store offset=40
      (get_local $3)
      (f64.const 0.6)
    )
    ;; raybench.cpp:277
    (f64.store offset=48
      (get_local $3)
      (f64.const 0.4)
    )
    ;; raybench.cpp:277
    (i32.store offset=56
      (get_local $3)
      (i32.const 10)
    )
    ;; raybench.cpp:277
    (f64.store offset=64
      (get_local $3)
      (f64.const 0.1)
    )
    ;; raybench.cpp:277
    (f64.store offset=72
      (get_local $3)
      (f64.const 0.1)
    )
    ;; raybench.cpp:277
    (f64.store offset=80
      (get_local $3)
      (f64.const 0.05)
    )
    ;; raybench.cpp:277
    (f64.store offset=88
      (get_local $3)
      (f64.const 0)
    )
    ;; raybench.cpp:453
    (i32.store
      (get_local $3)
      (i32.const 1596)
    )
    ;; raybench.cpp:451
    (f64.store offset=96
      (get_local $3)
      (f64.const 1.5)
    )
    ;; raybench.cpp:451
    (f64.store offset=104
      (get_local $3)
      (f64.const 1)
    )
    ;; raybench.cpp:451
    (f64.store offset=112
      (get_local $3)
      (f64.const 0)
    )
    ;; raybench.cpp:452
    (f64.store offset=120
      (get_local $3)
      (f64.const 0.75)
    )
    ;; raybench.cpp:927
    (i32.store
      (get_local $7)
      (get_local $3)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1604
    (set_local $6
      (i32.load
        (get_local $8)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1604
    (set_local $10
      (i32.load
        (tee_local $17
          (i32.add
            (get_local $9)
            (i32.const 8)
          )
        )
      )
    )
    (if
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
      (i32.lt_u
        (get_local $6)
        (get_local $10)
      )
      (block
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
        (i32.store
          (get_local $6)
          (get_local $3)
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1611
        (set_local $3
          (i32.load
            (get_local $8)
          )
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1611
        (i32.store
          (get_local $8)
          (i32.add
            (get_local $3)
            (i32.const 4)
          )
        )
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1614
      (call $__ZNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEE21__push_back_slow_pathIS2_EEvOT_
        (get_local $9)
        (get_local $7)
      )
    )
    ;; raybench.cpp:928
    (set_local $3
      (call $__Znwj
        (i32.const 192)
      )
    )
    ;; raybench.cpp:277
    (f64.store offset=8
      (get_local $3)
      (f64.const 0.1)
    )
    ;; raybench.cpp:277
    (f64.store offset=16
      (get_local $3)
      (f64.const 0.2)
    )
    ;; raybench.cpp:277
    (f64.store offset=24
      (get_local $3)
      (f64.const 0.2)
    )
    ;; raybench.cpp:277
    (f64.store offset=32
      (get_local $3)
      (f64.const 0.3)
    )
    ;; raybench.cpp:277
    (f64.store offset=40
      (get_local $3)
      (f64.const 0.6)
    )
    ;; raybench.cpp:277
    (f64.store offset=48
      (get_local $3)
      (f64.const 0.6)
    )
    ;; raybench.cpp:277
    (i32.store offset=56
      (get_local $3)
      (i32.const 10)
    )
    ;; raybench.cpp:277
    (f64.store offset=64
      (get_local $3)
      (f64.const 0.05)
    )
    ;; raybench.cpp:277
    (f64.store offset=72
      (get_local $3)
      (f64.const 0.1)
    )
    ;; raybench.cpp:277
    (f64.store offset=80
      (get_local $3)
      (f64.const 0.1)
    )
    ;; raybench.cpp:277
    (f64.store offset=88
      (get_local $3)
      (f64.const 0)
    )
    ;; raybench.cpp:511
    (i32.store
      (get_local $3)
      (i32.const 1512)
    )
    ;; raybench.cpp:507
    (f64.store offset=96
      (get_local $3)
      (f64.const -1)
    )
    ;; raybench.cpp:507
    (f64.store offset=104
      (get_local $3)
      (f64.const 0)
    )
    ;; raybench.cpp:507
    (f64.store offset=112
      (get_local $3)
      (f64.const 0.75)
    )
    ;; raybench.cpp:508
    (f64.store offset=120
      (get_local $3)
      (f64.const -0.75)
    )
    (i64.store
      (tee_local $6
        (i32.add
          (get_local $3)
          (i32.const 128)
        )
      )
      (i64.const 0)
    )
    ;; raybench.cpp:508
    (i64.store offset=8
      (get_local $6)
      (i64.const 0)
    )
    ;; raybench.cpp:509
    (f64.store offset=144
      (get_local $3)
      (f64.const -0.75)
    )
    ;; raybench.cpp:509
    (f64.store offset=152
      (get_local $3)
      (f64.const 1.5)
    )
    ;; raybench.cpp:509
    (f64.store offset=160
      (get_local $3)
      (f64.const 0)
    )
    ;; raybench.cpp:148
    (f64.store offset=168
      (get_local $3)
      (f64.const 0.9486832980505138)
    )
    ;; raybench.cpp:149
    (f64.store offset=176
      (get_local $3)
      (f64.const 0)
    )
    ;; raybench.cpp:150
    (f64.store offset=184
      (get_local $3)
      (f64.const 0.31622776601683794)
    )
    ;; raybench.cpp:928
    (i32.store
      (get_local $7)
      (get_local $3)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1604
    (set_local $6
      (i32.load
        (get_local $8)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1604
    (set_local $10
      (i32.load
        (get_local $17)
      )
    )
    (if
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
      (i32.lt_u
        (get_local $6)
        (get_local $10)
      )
      (block
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
        (i32.store
          (get_local $6)
          (get_local $3)
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1611
        (set_local $3
          (i32.load
            (get_local $8)
          )
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1611
        (i32.store
          (get_local $8)
          (i32.add
            (get_local $3)
            (i32.const 4)
          )
        )
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1614
      (call $__ZNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEE21__push_back_slow_pathIS2_EEvOT_
        (get_local $9)
        (get_local $7)
      )
    )
    ;; raybench.cpp:929
    (set_local $3
      (call $__Znwj
        (i32.const 192)
      )
    )
    ;; raybench.cpp:277
    (f64.store offset=8
      (get_local $3)
      (f64.const 0.1)
    )
    (i64.store
      (tee_local $6
        (i32.add
          (get_local $3)
          (i32.const 16)
        )
      )
      (i64.const 0)
    )
    ;; raybench.cpp:277
    (i64.store offset=8
      (get_local $6)
      (i64.const 0)
    )
    ;; raybench.cpp:277
    (f64.store offset=32
      (get_local $3)
      (f64.const 0.8)
    )
    (i64.store
      (tee_local $6
        (i32.add
          (get_local $3)
          (i32.const 40)
        )
      )
      (i64.const 0)
    )
    ;; raybench.cpp:277
    (i64.store offset=8
      (get_local $6)
      (i64.const 0)
    )
    ;; raybench.cpp:277
    (i32.store offset=56
      (get_local $3)
      (i32.const 10)
    )
    ;; raybench.cpp:277
    (f64.store offset=64
      (get_local $3)
      (f64.const 0.1)
    )
    (i64.store
      (tee_local $6
        (i32.add
          (get_local $3)
          (i32.const 72)
        )
      )
      (i64.const 0)
    )
    (i64.store offset=8
      (get_local $6)
      (i64.const 0)
    )
    ;; raybench.cpp:277
    (i64.store offset=16
      (get_local $6)
      (i64.const 0)
    )
    ;; raybench.cpp:511
    (i32.store
      (get_local $3)
      (i32.const 1512)
    )
    ;; raybench.cpp:507
    (f64.store offset=96
      (get_local $3)
      (f64.const -2)
    )
    (i64.store
      (tee_local $6
        (i32.add
          (get_local $3)
          (i32.const 104)
        )
      )
      (i64.const 0)
    )
    ;; raybench.cpp:507
    (i64.store offset=8
      (get_local $6)
      (i64.const 0)
    )
    ;; raybench.cpp:508
    (f64.store offset=120
      (get_local $3)
      (f64.const -0.5)
    )
    (i64.store
      (tee_local $6
        (i32.add
          (get_local $3)
          (i32.const 128)
        )
      )
      (i64.const 0)
    )
    ;; raybench.cpp:508
    (i64.store offset=8
      (get_local $6)
      (i64.const 0)
    )
    ;; raybench.cpp:509
    (f64.store offset=144
      (get_local $3)
      (f64.const -0.5)
    )
    ;; raybench.cpp:509
    (f64.store offset=152
      (get_local $3)
      (f64.const 2)
    )
    (i64.store
      (tee_local $6
        (i32.add
          (get_local $3)
          (i32.const 160)
        )
      )
      (i64.const 0)
    )
    (i64.store offset=8
      (get_local $6)
      (i64.const 0)
    )
    ;; raybench.cpp:148
    (i64.store offset=16
      (get_local $6)
      (i64.const 0)
    )
    ;; raybench.cpp:150
    (f64.store offset=184
      (get_local $3)
      (f64.const 1)
    )
    ;; raybench.cpp:929
    (i32.store
      (get_local $7)
      (get_local $3)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1604
    (set_local $6
      (i32.load
        (get_local $8)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1604
    (set_local $10
      (i32.load
        (get_local $17)
      )
    )
    (if
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
      (i32.lt_u
        (get_local $6)
        (get_local $10)
      )
      (block
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
        (i32.store
          (get_local $6)
          (get_local $3)
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1611
        (set_local $3
          (i32.load
            (get_local $8)
          )
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1611
        (i32.store
          (get_local $8)
          (i32.add
            (get_local $3)
            (i32.const 4)
          )
        )
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1614
      (call $__ZNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEE21__push_back_slow_pathIS2_EEvOT_
        (get_local $9)
        (get_local $7)
      )
    )
    ;; raybench.cpp:148
    (f64.store
      (get_local $7)
      (f64.const -5)
    )
    ;; raybench.cpp:149
    (f64.store offset=8
      (get_local $7)
      (f64.const 0)
    )
    ;; raybench.cpp:150
    (f64.store offset=16
      (get_local $7)
      (f64.const 5)
    )
    ;; raybench.cpp:148
    (f64.store
      (tee_local $6
        (i32.add
          (get_local $12)
          (i32.const 192)
        )
      )
      (f64.const 5)
    )
    ;; raybench.cpp:149
    (f64.store offset=8
      (get_local $6)
      (f64.const 0)
    )
    ;; raybench.cpp:150
    (f64.store offset=16
      (get_local $6)
      (f64.const 5)
    )
    ;; raybench.cpp:148
    (f64.store
      (tee_local $10
        (i32.add
          (get_local $12)
          (i32.const 168)
        )
      )
      (f64.const 5)
    )
    ;; raybench.cpp:149
    (f64.store offset=8
      (get_local $10)
      (f64.const 0)
    )
    ;; raybench.cpp:150
    (f64.store offset=16
      (get_local $10)
      (f64.const -40)
    )
    ;; raybench.cpp:148
    (f64.store
      (tee_local $14
        (i32.add
          (get_local $12)
          (i32.const 96)
        )
      )
      (f64.const -5)
    )
    ;; raybench.cpp:149
    (f64.store offset=8
      (get_local $14)
      (f64.const 0)
    )
    ;; raybench.cpp:150
    (f64.store offset=16
      (get_local $14)
      (f64.const -40)
    )
    ;; raybench.cpp:930
    (call $__Z9rectangleRNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEEERK8MaterialRK4Vec3SC_SC_SC_
      (get_local $9)
      (get_local $5)
      (get_local $7)
      (get_local $6)
      (get_local $10)
      (get_local $14)
    )
    ;; raybench.cpp:148
    (f64.store
      (get_local $7)
      (f64.const 1)
    )
    ;; raybench.cpp:149
    (f64.store offset=8
      (get_local $7)
      (f64.const 1.5)
    )
    ;; raybench.cpp:150
    (f64.store offset=16
      (get_local $7)
      (f64.const 1.5)
    )
    ;; raybench.cpp:148
    (f64.store
      (get_local $6)
      (f64.const 1.5)
    )
    ;; raybench.cpp:149
    (f64.store offset=8
      (get_local $6)
      (f64.const 1.5)
    )
    ;; raybench.cpp:150
    (f64.store offset=16
      (get_local $6)
      (f64.const 1.25)
    )
    ;; raybench.cpp:148
    (f64.store
      (get_local $10)
      (f64.const 1.5)
    )
    ;; raybench.cpp:149
    (f64.store offset=8
      (get_local $10)
      (f64.const 1.75)
    )
    ;; raybench.cpp:150
    (f64.store offset=16
      (get_local $10)
      (f64.const 1.25)
    )
    ;; raybench.cpp:148
    (f64.store
      (get_local $14)
      (f64.const 1)
    )
    ;; raybench.cpp:149
    (f64.store offset=8
      (get_local $14)
      (f64.const 1.75)
    )
    ;; raybench.cpp:150
    (f64.store offset=16
      (get_local $14)
      (f64.const 1.5)
    )
    ;; raybench.cpp:148
    (f64.store
      (tee_local $5
        (i32.add
          (get_local $12)
          (i32.const 72)
        )
      )
      (f64.const 1.5)
    )
    ;; raybench.cpp:149
    (f64.store offset=8
      (get_local $5)
      (f64.const 1.5)
    )
    ;; raybench.cpp:150
    (f64.store offset=16
      (get_local $5)
      (f64.const 0.5)
    )
    ;; raybench.cpp:148
    (f64.store
      (tee_local $18
        (i32.add
          (get_local $12)
          (i32.const 48)
        )
      )
      (f64.const 1)
    )
    ;; raybench.cpp:149
    (f64.store offset=8
      (get_local $18)
      (f64.const 1.5)
    )
    ;; raybench.cpp:150
    (f64.store offset=16
      (get_local $18)
      (f64.const 0.75)
    )
    ;; raybench.cpp:148
    (f64.store
      (tee_local $19
        (i32.add
          (get_local $12)
          (i32.const 24)
        )
      )
      (f64.const 1)
    )
    ;; raybench.cpp:149
    (f64.store offset=8
      (get_local $19)
      (f64.const 1.75)
    )
    ;; raybench.cpp:150
    (f64.store offset=16
      (get_local $19)
      (f64.const 0.75)
    )
    ;; raybench.cpp:148
    (f64.store
      (tee_local $3
        (get_local $12)
      )
      (f64.const 1.5)
    )
    ;; raybench.cpp:149
    (f64.store offset=8
      (get_local $3)
      (f64.const 1.75)
    )
    ;; raybench.cpp:150
    (f64.store offset=16
      (get_local $3)
      (f64.const 0.5)
    )
    ;; raybench.cpp:824
    (call $__Z9rectangleRNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEEERK8MaterialRK4Vec3SC_SC_SC_
      (get_local $9)
      (get_local $4)
      (get_local $7)
      (get_local $6)
      (get_local $10)
      (get_local $14)
    )
    ;; raybench.cpp:825
    (call $__Z9rectangleRNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEEERK8MaterialRK4Vec3SC_SC_SC_
      (get_local $9)
      (get_local $4)
      (get_local $6)
      (get_local $5)
      (get_local $3)
      (get_local $10)
    )
    ;; raybench.cpp:826
    (call $__Z9rectangleRNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEEERK8MaterialRK4Vec3SC_SC_SC_
      (get_local $9)
      (get_local $4)
      (get_local $18)
      (get_local $7)
      (get_local $14)
      (get_local $19)
    )
    ;; raybench.cpp:827
    (call $__Z9rectangleRNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEEERK8MaterialRK4Vec3SC_SC_SC_
      (get_local $9)
      (get_local $4)
      (get_local $5)
      (get_local $18)
      (get_local $19)
      (get_local $3)
    )
    ;; raybench.cpp:828
    (call $__Z9rectangleRNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEEERK8MaterialRK4Vec3SC_SC_SC_
      (get_local $9)
      (get_local $4)
      (get_local $14)
      (get_local $10)
      (get_local $3)
      (get_local $19)
    )
    ;; raybench.cpp:829
    (call $__Z9rectangleRNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEEERK8MaterialRK4Vec3SC_SC_SC_
      (get_local $9)
      (get_local $4)
      (get_local $18)
      (get_local $7)
      (get_local $6)
      (get_local $5)
    )
    (set_local $3
      (i32.const 0)
    )
    (loop $while-in
      ;; raybench.cpp:934
      (set_local $4
        (call $__Znwj
          (i32.const 128)
        )
      )
      ;; raybench.cpp:934
      (set_local $15
        (call $_cos
          (f64.div
            (tee_local $13
              (f64.convert_u/i32
                (get_local $3)
              )
            )
            (f64.const 30)
          )
        )
      )
      ;; raybench.cpp:277
      (f64.store offset=8
        (get_local $4)
        (f64.const 0.6)
      )
      ;; raybench.cpp:277
      (f64.store offset=16
        (get_local $4)
        (f64.const 0.6)
      )
      (i64.store
        (tee_local $5
          (i32.add
            (get_local $4)
            (i32.const 24)
          )
        )
        (i64.const 0)
      )
      (i64.store offset=8
        (get_local $5)
        (i64.const 0)
      )
      (i64.store offset=16
        (get_local $5)
        (i64.const 0)
      )
      (i64.store offset=24
        (get_local $5)
        (i64.const 0)
      )
      ;; raybench.cpp:277
      (i32.store offset=32
        (get_local $5)
        (i32.const 0)
      )
      ;; raybench.cpp:277
      (f64.store offset=64
        (get_local $4)
        (f64.const 0.4)
      )
      ;; raybench.cpp:277
      (f64.store offset=72
        (get_local $4)
        (f64.const 0.4)
      )
      (i64.store
        (tee_local $5
          (i32.add
            (get_local $4)
            (i32.const 80)
          )
        )
        (i64.const 0)
      )
      ;; raybench.cpp:277
      (i64.store offset=8
        (get_local $5)
        (i64.const 0)
      )
      ;; raybench.cpp:453
      (i32.store
        (get_local $4)
        (i32.const 1596)
      )
      ;; raybench.cpp:451
      (f64.store offset=96
        (get_local $4)
        (f64.add
          (f64.mul
            (get_local $13)
            (f64.const 0.2)
          )
          (f64.const -0.6)
        )
      )
      ;; raybench.cpp:451
      (f64.store offset=104
        (get_local $4)
        (f64.add
          (f64.mul
            (get_local $13)
            (f64.const 0.05)
          )
          (f64.const 0.075)
        )
      )
      ;; raybench.cpp:451
      (f64.store offset=112
        (get_local $4)
        (f64.sub
          (f64.const 1.5)
          (f64.mul
            (f64.mul
              (get_local $13)
              (get_local $15)
            )
            (f64.const 0.5)
          )
        )
      )
      ;; raybench.cpp:452
      (f64.store offset=120
        (get_local $4)
        (f64.const 0.075)
      )
      ;; raybench.cpp:934
      (i32.store
        (get_local $7)
        (get_local $4)
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1604
      (set_local $5
        (i32.load
          (get_local $8)
        )
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1604
      (set_local $6
        (i32.load
          (get_local $17)
        )
      )
      ;; raybench.cpp:933
      (if
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
        (i32.lt_u
          (get_local $5)
          (get_local $6)
        )
        (block
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
          (i32.store
            (get_local $5)
            (get_local $4)
          )
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1611
          (set_local $4
            (i32.load
              (get_local $8)
            )
          )
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1611
          (i32.store
            (get_local $8)
            (i32.add
              (get_local $4)
              (i32.const 4)
            )
          )
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1614
        (call $__ZNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEE21__push_back_slow_pathIS2_EEvOT_
          (get_local $9)
          (get_local $7)
        )
      )
      (br_if $while-in
        (i32.lt_u
          (tee_local $3
            (i32.add
              (get_local $3)
              (i32.const 1)
            )
          )
          (i32.const 30)
        )
      )
    )
    (set_local $3
      (i32.const 0)
    )
    (loop $while-in1
      ;; raybench.cpp:936
      (set_local $4
        (call $__Znwj
          (i32.const 128)
        )
      )
      ;; raybench.cpp:936
      (set_local $16
        (call $_sin
          (tee_local $15
            (f64.mul
              (tee_local $13
                (f64.convert_u/i32
                  (get_local $3)
                )
              )
              (f64.const 0.19625)
            )
          )
        )
      )
      ;; raybench.cpp:936
      (set_local $15
        (call $_cos
          (get_local $15)
        )
      )
      ;; raybench.cpp:277
      (f64.store offset=8
        (get_local $4)
        (f64.const 0.6)
      )
      (i64.store
        (tee_local $5
          (i32.add
            (get_local $4)
            (i32.const 16)
          )
        )
        (i64.const 0)
      )
      (i64.store offset=8
        (get_local $5)
        (i64.const 0)
      )
      (i64.store offset=16
        (get_local $5)
        (i64.const 0)
      )
      (i64.store offset=24
        (get_local $5)
        (i64.const 0)
      )
      (i64.store offset=32
        (get_local $5)
        (i64.const 0)
      )
      ;; raybench.cpp:277
      (i32.store offset=40
        (get_local $5)
        (i32.const 0)
      )
      ;; raybench.cpp:277
      (f64.store offset=64
        (get_local $4)
        (f64.const 0.4)
      )
      (i64.store
        (tee_local $5
          (i32.add
            (get_local $4)
            (i32.const 72)
          )
        )
        (i64.const 0)
      )
      (i64.store offset=8
        (get_local $5)
        (i64.const 0)
      )
      ;; raybench.cpp:277
      (i64.store offset=16
        (get_local $5)
        (i64.const 0)
      )
      ;; raybench.cpp:453
      (i32.store
        (get_local $4)
        (i32.const 1596)
      )
      ;; raybench.cpp:451
      (f64.store offset=96
        (get_local $4)
        (f64.add
          (f64.mul
            (get_local $16)
            (f64.const 0.3)
          )
          (f64.const 1)
        )
      )
      ;; raybench.cpp:451
      (f64.store offset=104
        (get_local $4)
        (f64.add
          (f64.mul
            (get_local $13)
            (f64.const 0.025)
          )
          (f64.const 0.075)
        )
      )
      ;; raybench.cpp:451
      (f64.store offset=112
        (get_local $4)
        (f64.add
          (f64.mul
            (get_local $15)
            (f64.const 0.3)
          )
          (f64.const 1)
        )
      )
      ;; raybench.cpp:452
      (f64.store offset=120
        (get_local $4)
        (f64.const 0.025)
      )
      ;; raybench.cpp:936
      (i32.store
        (get_local $7)
        (get_local $4)
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1604
      (set_local $5
        (i32.load
          (get_local $8)
        )
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1604
      (set_local $6
        (i32.load
          (get_local $17)
        )
      )
      ;; raybench.cpp:935
      (if
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
        (i32.lt_u
          (get_local $5)
          (get_local $6)
        )
        (block
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
          (i32.store
            (get_local $5)
            (get_local $4)
          )
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1611
          (set_local $4
            (i32.load
              (get_local $8)
            )
          )
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1611
          (i32.store
            (get_local $8)
            (i32.add
              (get_local $4)
              (i32.const 4)
            )
          )
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1614
        (call $__ZNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEE21__push_back_slow_pathIS2_EEvOT_
          (get_local $9)
          (get_local $7)
        )
      )
      (br_if $while-in1
        (i32.lt_u
          (tee_local $3
            (i32.add
              (get_local $3)
              (i32.const 1)
            )
          )
          (i32.const 60)
        )
      )
    )
    (set_local $3
      (i32.const 0)
    )
    (loop $while-in3
      ;; raybench.cpp:938
      (set_local $4
        (call $__Znwj
          (i32.const 128)
        )
      )
      ;; raybench.cpp:938
      (set_local $15
        (call $_sin
          (tee_local $13
            (f64.mul
              (f64.convert_u/i32
                (get_local $3)
              )
              (f64.const 0.19625)
            )
          )
        )
      )
      ;; raybench.cpp:938
      (set_local $13
        (call $_cos
          (get_local $13)
        )
      )
      (i64.store
        (tee_local $5
          (i32.add
            (get_local $4)
            (i32.const 8)
          )
        )
        (i64.const 0)
      )
      ;; raybench.cpp:277
      (i64.store offset=8
        (get_local $5)
        (i64.const 0)
      )
      ;; raybench.cpp:277
      (f64.store offset=24
        (get_local $4)
        (f64.const 0.6)
      )
      (i64.store
        (tee_local $5
          (i32.add
            (get_local $4)
            (i32.const 32)
          )
        )
        (i64.const 0)
      )
      (i64.store offset=8
        (get_local $5)
        (i64.const 0)
      )
      (i64.store offset=16
        (get_local $5)
        (i64.const 0)
      )
      ;; raybench.cpp:277
      (i32.store offset=24
        (get_local $5)
        (i32.const 0)
      )
      (i64.store
        (tee_local $5
          (i32.add
            (get_local $4)
            (i32.const 64)
          )
        )
        (i64.const 0)
      )
      ;; raybench.cpp:277
      (i64.store offset=8
        (get_local $5)
        (i64.const 0)
      )
      ;; raybench.cpp:277
      (f64.store offset=80
        (get_local $4)
        (f64.const 0.4)
      )
      ;; raybench.cpp:277
      (f64.store offset=88
        (get_local $4)
        (f64.const 0)
      )
      ;; raybench.cpp:453
      (i32.store
        (get_local $4)
        (i32.const 1596)
      )
      ;; raybench.cpp:451
      (f64.store offset=96
        (get_local $4)
        (f64.add
          (f64.mul
            (get_local $15)
            (f64.const 0.3)
          )
          (f64.const 1)
        )
      )
      ;; raybench.cpp:451
      (f64.store offset=104
        (get_local $4)
        (f64.add
          (f64.mul
            (f64.convert_u/i32
              (i32.add
                (get_local $3)
                (i32.const 8)
              )
            )
            (f64.const 0.025)
          )
          (f64.const 0.075)
        )
      )
      ;; raybench.cpp:451
      (f64.store offset=112
        (get_local $4)
        (f64.add
          (f64.mul
            (get_local $13)
            (f64.const 0.3)
          )
          (f64.const 1)
        )
      )
      ;; raybench.cpp:452
      (f64.store offset=120
        (get_local $4)
        (f64.const 0.025)
      )
      ;; raybench.cpp:938
      (i32.store
        (get_local $7)
        (get_local $4)
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1604
      (set_local $5
        (i32.load
          (get_local $8)
        )
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1604
      (set_local $6
        (i32.load
          (get_local $17)
        )
      )
      ;; raybench.cpp:937
      (if
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
        (i32.lt_u
          (get_local $5)
          (get_local $6)
        )
        (block
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
          (i32.store
            (get_local $5)
            (get_local $4)
          )
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1611
          (set_local $4
            (i32.load
              (get_local $8)
            )
          )
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1611
          (i32.store
            (get_local $8)
            (i32.add
              (get_local $4)
              (i32.const 4)
            )
          )
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1614
        (call $__ZNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEE21__push_back_slow_pathIS2_EEvOT_
          (get_local $9)
          (get_local $7)
        )
      )
      (br_if $while-in3
        (i32.lt_u
          (tee_local $3
            (i32.add
              (get_local $3)
              (i32.const 1)
            )
          )
          (i32.const 60)
        )
      )
    )
    ;; raybench.cpp:940
    (f64.store
      (get_local $0)
      (f64.const 0.5)
    )
    ;; raybench.cpp:940
    (f64.store offset=8
      (get_local $0)
      (f64.const 0.75)
    )
    ;; raybench.cpp:940
    (f64.store offset=16
      (get_local $0)
      (f64.const 5)
    )
    ;; raybench.cpp:941
    (f64.store
      (get_local $1)
      (f64.const -3)
    )
    ;; raybench.cpp:941
    (f64.store offset=8
      (get_local $1)
      (f64.const 1.5)
    )
    ;; raybench.cpp:941
    (f64.store offset=16
      (get_local $1)
      (f64.const 2)
    )
    ;; raybench.cpp:942
    (f64.store
      (get_local $2)
      (f64.const 0.09765625)
    )
    ;; raybench.cpp:942
    (f64.store offset=8
      (get_local $2)
      (f64.const 0.09765625)
    )
    ;; raybench.cpp:942
    (f64.store offset=16
      (get_local $2)
      (f64.const 0.4375)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1466
    (set_local $0
      (i32.load
        (get_local $9)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1482
    (set_local $1
      (i32.load
        (get_local $8)
      )
    )
    (if
      (i32.eq
        (get_local $0)
        (get_local $1)
      )
      (block
        (set_local $13
          (f64.const 1.e+100)
        )
        (set_local $15
          (f64.const -1.e+100)
        )
        (set_local $20
          (f64.const -1.e+100)
        )
        (set_local $21
          (f64.const 1.e+100)
        )
        (set_local $22
          (f64.const -1.e+100)
        )
        (set_local $16
          (f64.const 1.e+100)
        )
      )
      (block
        ;; raybench.cpp:842
        (set_local $3
          (i32.add
            (get_local $7)
            (i32.const 8)
          )
        )
        ;; raybench.cpp:843
        (set_local $4
          (i32.add
            (get_local $7)
            (i32.const 16)
          )
        )
        ;; raybench.cpp:844
        (set_local $5
          (i32.add
            (get_local $7)
            (i32.const 24)
          )
        )
        ;; raybench.cpp:845
        (set_local $6
          (i32.add
            (get_local $7)
            (i32.const 32)
          )
        )
        ;; raybench.cpp:846
        (set_local $10
          (i32.add
            (get_local $7)
            (i32.const 40)
          )
        )
        (set_local $15
          (f64.const -1.e+100)
        )
        (set_local $20
          (f64.const -1.e+100)
        )
        (set_local $21
          (f64.const 1.e+100)
        )
        (set_local $22
          (f64.const -1.e+100)
        )
        (set_local $16
          (f64.const 1.e+100)
        )
        (set_local $13
          (f64.const 1.e+100)
        )
        (loop $while-in5
          ;; raybench.cpp:839
          (set_local $2
            (i32.load
              (get_local $0)
            )
          )
          ;; raybench.cpp:840
          (set_local $14
            (i32.load
              (get_local $2)
            )
          )
          ;; raybench.cpp:840
          (set_local $14
            (i32.load offset=8
              (get_local $14)
            )
          )
          ;; raybench.cpp:840
          (call_indirect $FUNCSIG$vii
            (get_local $7)
            (get_local $2)
            (i32.add
              (i32.and
                (get_local $14)
                (i32.const 15)
              )
              (i32.const 36)
            )
          )
          ;; raybench.cpp:841
          (set_local $11
            (f64.load
              (get_local $7)
            )
          )
          ;; raybench.cpp:221
          (if
            (i32.eqz
              (f64.lt
                (get_local $13)
                (get_local $11)
              )
            )
            (set_local $13
              (get_local $11)
            )
          )
          ;; raybench.cpp:842
          (set_local $11
            (f64.load
              (get_local $3)
            )
          )
          ;; raybench.cpp:212
          (if
            (i32.eqz
              (f64.gt
                (get_local $15)
                (get_local $11)
              )
            )
            (set_local $15
              (get_local $11)
            )
          )
          ;; raybench.cpp:843
          (set_local $11
            (f64.load
              (get_local $4)
            )
          )
          ;; raybench.cpp:221
          (if
            (i32.eqz
              (f64.lt
                (get_local $16)
                (get_local $11)
              )
            )
            (set_local $16
              (get_local $11)
            )
          )
          ;; raybench.cpp:844
          (set_local $11
            (f64.load
              (get_local $5)
            )
          )
          ;; raybench.cpp:212
          (if
            (i32.eqz
              (f64.gt
                (get_local $22)
                (get_local $11)
              )
            )
            (set_local $22
              (get_local $11)
            )
          )
          ;; raybench.cpp:845
          (set_local $11
            (f64.load
              (get_local $6)
            )
          )
          ;; raybench.cpp:221
          (if
            (i32.eqz
              (f64.lt
                (get_local $21)
                (get_local $11)
              )
            )
            (set_local $21
              (get_local $11)
            )
          )
          ;; raybench.cpp:846
          (set_local $11
            (f64.load
              (get_local $10)
            )
          )
          ;; raybench.cpp:212
          (if
            (i32.eqz
              (f64.gt
                (get_local $20)
                (get_local $11)
              )
            )
            (set_local $20
              (get_local $11)
            )
          )
          (br_if $while-in5
            (i32.ne
              (tee_local $0
                (i32.add
                  (get_local $0)
                  (i32.const 4)
                )
              )
              (get_local $1)
            )
          )
        )
      )
    )
    ;; raybench.cpp:262
    (f64.store
      (tee_local $0
        (i32.add
          (get_local $12)
          (i32.const 120)
        )
      )
      (get_local $13)
    )
    ;; raybench.cpp:263
    (f64.store offset=8
      (get_local $0)
      (get_local $15)
    )
    ;; raybench.cpp:264
    (f64.store offset=16
      (get_local $0)
      (get_local $16)
    )
    ;; raybench.cpp:265
    (f64.store offset=24
      (get_local $0)
      (get_local $22)
    )
    ;; raybench.cpp:266
    (f64.store offset=32
      (get_local $0)
      (get_local $21)
    )
    ;; raybench.cpp:267
    (f64.store offset=40
      (get_local $0)
      (get_local $20)
    )
    (i64.store
      (get_local $7)
      (i64.load
        (get_local $0)
      )
    )
    (i64.store offset=8
      (get_local $7)
      (i64.load offset=8
        (get_local $0)
      )
    )
    (i64.store offset=16
      (get_local $7)
      (i64.load offset=16
        (get_local $0)
      )
    )
    (i64.store offset=24
      (get_local $7)
      (i64.load offset=24
        (get_local $0)
      )
    )
    (i64.store offset=32
      (get_local $7)
      (i64.load offset=32
        (get_local $0)
      )
    )
    ;; raybench.cpp:945
    (i64.store offset=40
      (get_local $7)
      (i64.load offset=40
        (get_local $0)
      )
    )
    ;; raybench.cpp:945
    (set_local $3
      (call $__Z9partitionRNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEEE6Boundsj
        (get_local $9)
        (get_local $7)
        (i32.const 0)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:450
    (set_local $0
      (i32.load
        (get_local $9)
      )
    )
    (if
      (i32.eqz
        (get_local $0)
      )
      (block
        (set_global $STACKTOP
          (get_local $12)
        )
        (return
          (get_local $3)
        )
      )
    )
    (set_local $1
      (get_local $0)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:424
    (set_local $2
      (i32.load
        (get_local $8)
      )
    )
    (if
      (i32.ne
        (get_local $2)
        (get_local $0)
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:425
      (i32.store
        (get_local $8)
        (i32.add
          (get_local $2)
          (i32.shl
            (i32.xor
              (i32.shr_u
                (i32.sub
                  (i32.add
                    (get_local $2)
                    (i32.const -4)
                  )
                  (get_local $1)
                )
                (i32.const 2)
              )
              (i32.const -1)
            )
            (i32.const 2)
          )
        )
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/new:176
    (call $__ZdlPv
      (get_local $0)
    )
    (set_global $STACKTOP
      (get_local $12)
    )
    (get_local $3)
  )
  (func $__Z18traceWithAntialiasjjjj (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32)
    (local $4 i32)
    (local $5 i32)
    (local $6 i32)
    (local $7 i32)
    (local $8 i32)
    (local $9 i32)
    (local $10 i32)
    (local $11 i32)
    (local $12 i32)
    (local $13 i32)
    (local $14 f64)
    (local $15 f64)
    (local $16 f64)
    (local $17 f64)
    (local $18 f64)
    (local $19 i32)
    (local $20 i32)
    (local $21 i32)
    (local $22 i32)
    (local $23 i32)
    (local $24 f64)
    (local $25 f64)
    (local $26 f64)
    (local $27 f64)
    (local $28 i32)
    (local $29 i32)
    (set_local $5
      (get_global $STACKTOP)
    )
    ;; raybench.cpp:736
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 48)
      )
    )
    ;; raybench.cpp:737
    (if
      (i32.ge_u
        (get_local $0)
        (get_local $1)
      )
      (block
        (set_global $STACKTOP
          (get_local $5)
        )
        (return)
      )
    )
    (if
      (i32.le_u
        (get_local $3)
        (get_local $2)
      )
      (block
        (set_global $STACKTOP
          (get_local $5)
        )
        (return)
      )
    )
    (set_local $19
      (i32.add
        (tee_local $10
          (i32.add
            (get_local $5)
            (i32.const 24)
          )
        )
        (i32.const 8)
      )
    )
    (set_local $20
      (i32.add
        (get_local $10)
        (i32.const 16)
      )
    )
    (set_local $21
      (i32.add
        (tee_local $11
          (get_local $5)
        )
        (i32.const 8)
      )
    )
    (set_local $22
      (i32.add
        (get_local $11)
        (i32.const 16)
      )
    )
    ;; raybench.cpp:737
    (set_local $23
      (i32.sub
        (get_local $3)
        (get_local $2)
      )
    )
    (set_local $12
      (i32.const 0)
    )
    (loop $while-in
      ;; raybench.cpp:749
      (set_local $24
        (f64.convert_u/i32
          (get_local $0)
        )
      )
      (set_local $9
        (get_local $2)
      )
      ;; raybench.cpp:741
      (set_local $13
        (get_local $12)
      )
      (loop $while-in1
        ;; raybench.cpp:748
        (set_local $25
          (f64.convert_u/i32
            (get_local $9)
          )
        )
        (set_local $6
          (i32.const 0)
        )
        (set_local $7
          (i32.and
            (get_local $13)
            (i32.const 1)
          )
        )
        (set_local $14
          (f64.const 0)
        )
        (set_local $15
          (f64.const 0)
        )
        (set_local $16
          (f64.const 0)
        )
        (loop $while-in3
          ;; raybench.cpp:748
          (set_local $26
            (f64.convert_u/i32
              (get_local $6)
            )
          )
          (set_local $8
            (i32.const 0)
          )
          ;; raybench.cpp:746
          (set_local $4
            (get_local $7)
          )
          (loop $while-in5
            ;; raybench.cpp:746
            (set_local $17
              (f64.load
                (i32.add
                  (i32.shl
                    (get_local $4)
                    (i32.const 3)
                  )
                  (i32.const 1024)
                )
              )
            )
            ;; raybench.cpp:747
            (set_local $18
              (f64.load
                (i32.add
                  (i32.shl
                    (i32.add
                      (get_local $4)
                      (i32.const 1)
                    )
                    (i32.const 3)
                  )
                  (i32.const 1024)
                )
              )
            )
            ;; raybench.cpp:747
            (set_local $4
              (i32.add
                (get_local $4)
                (i32.const 2)
              )
            )
            ;; raybench.cpp:750
            (set_local $27
              (f64.load
                (i32.const 5432)
              )
            )
            ;; raybench.cpp:148
            (f64.store
              (get_local $10)
              (f64.add
                (f64.div
                  (f64.mul
                    (f64.add
                      (get_local $25)
                      (f64.mul
                        (f64.add
                          (get_local $26)
                          (get_local $17)
                        )
                        (f64.const 0.25)
                      )
                    )
                    (f64.const 4)
                  )
                  (f64.const 800)
                )
                (f64.const -2)
              )
            )
            ;; raybench.cpp:149
            (f64.store
              (get_local $19)
              (f64.add
                (f64.div
                  (f64.mul
                    (f64.add
                      (get_local $24)
                      (f64.mul
                        (f64.add
                          (f64.convert_u/i32
                            (get_local $8)
                          )
                          (get_local $18)
                        )
                        (f64.const 0.25)
                      )
                    )
                    (f64.const 3)
                  )
                  (f64.const 600)
                )
                (f64.const -1.5)
              )
            )
            ;; raybench.cpp:150
            (f64.store
              (get_local $20)
              (f64.neg
                (get_local $27)
              )
            )
            ;; raybench.cpp:751
            (call $__Z8raycolorRK4Vec3S1_ddj
              (get_local $11)
              (i32.const 5416)
              (get_local $10)
              (f64.const 0)
              (f64.const 1.e+32)
              (i32.const 2)
            )
            (set_local $17
              (f64.load
                (get_local $21)
              )
            )
            (set_local $18
              (f64.load
                (get_local $22)
              )
            )
            ;; raybench.cpp:163
            (set_local $14
              (f64.add
                (get_local $14)
                (f64.load
                  (get_local $11)
                )
              )
            )
            ;; raybench.cpp:163
            (set_local $15
              (f64.add
                (get_local $15)
                (get_local $17)
              )
            )
            ;; raybench.cpp:163
            (set_local $16
              (f64.add
                (get_local $16)
                (get_local $18)
              )
            )
            (br_if $while-in5
              (i32.ne
                (tee_local $8
                  (i32.add
                    (get_local $8)
                    (i32.const 1)
                  )
                )
                (i32.const 4)
              )
            )
          )
          ;; raybench.cpp:745
          (set_local $7
            (i32.add
              (get_local $7)
              (i32.const 8)
            )
          )
          (br_if $while-in3
            (i32.ne
              (tee_local $6
                (i32.add
                  (get_local $6)
                  (i32.const 1)
                )
              )
              (i32.const 4)
            )
          )
        )
        ;; raybench.cpp:743
        (set_local $13
          (i32.add
            (get_local $13)
            (i32.const 1)
          )
        )
        ;; raybench.cpp:754
        (set_local $4
          (i32.load
            (i32.const 5492)
          )
        )
        ;; raybench.cpp:576
        (set_local $7
          (call $f64-to-int
            (f64.mul
              (f64.mul
                (get_local $16)
                (f64.const 0.0625)
              )
              (f64.const 255)
            )
          )
        )
        ;; raybench.cpp:576
        (set_local $6
          (call $f64-to-int
            (f64.mul
              (f64.mul
                (get_local $15)
                (f64.const 0.0625)
              )
              (f64.const 255)
            )
          )
        )
        ;; raybench.cpp:576
        (set_local $8
          (call $f64-to-int
            (f64.mul
              (f64.mul
                (get_local $14)
                (f64.const 0.0625)
              )
              (f64.const 255)
            )
          )
        )
        ;; raybench.cpp:614
        (set_local $28
          (i32.load
            (get_local $4)
          )
        )
        ;; raybench.cpp:614
        (set_local $29
          (i32.load offset=4
            (get_local $4)
          )
        )
        ;; raybench.cpp:614
        (set_local $4
          (i32.load offset=8
            (get_local $4)
          )
        )
        ;; raybench.cpp:614
        (i32.store
          (i32.add
            (get_local $4)
            (i32.shl
              (i32.add
                (i32.mul
                  (i32.sub
                    (get_local $28)
                    (get_local $0)
                  )
                  (get_local $29)
                )
                (get_local $9)
              )
              (i32.const 2)
            )
          )
          (i32.or
            (i32.or
              (i32.or
                (i32.shl
                  (get_local $7)
                  (i32.const 16)
                )
                (i32.shl
                  (get_local $6)
                  (i32.const 8)
                )
              )
              (get_local $8)
            )
            (i32.const -16777216)
          )
        )
        (br_if $while-in1
          (i32.ne
            (tee_local $9
              (i32.add
                (get_local $9)
                (i32.const 1)
              )
            )
            (get_local $3)
          )
        )
      )
      ;; raybench.cpp:737
      (set_local $12
        (i32.add
          (get_local $23)
          (get_local $12)
        )
      )
      (br_if $while-in
        (i32.ne
          (tee_local $0
            (i32.add
              (get_local $0)
              (i32.const 1)
            )
          )
          (get_local $1)
        )
      )
    )
    (set_global $STACKTOP
      (get_local $5)
    )
  )
  (func $__Z8raycolorRK4Vec3S1_ddj (param $0 i32) (param $1 i32) (param $2 i32) (param $3 f64) (param $4 f64) (param $5 i32)
    (local $6 f64)
    (local $7 i32)
    (local $8 f64)
    (local $9 f64)
    (local $10 f64)
    (local $11 f64)
    (local $12 i32)
    (local $13 f64)
    (local $14 f64)
    (local $15 i32)
    (local $16 i32)
    (local $17 i32)
    (local $18 i32)
    (local $19 f64)
    (local $20 f64)
    (local $21 i32)
    (local $22 i32)
    (local $23 i32)
    (local $24 i32)
    (local $25 i32)
    (local $26 i32)
    (local $27 i32)
    (local $28 i32)
    (local $29 f64)
    (local $30 f64)
    (local $31 f64)
    (local $32 f64)
    (local $33 f64)
    (set_local $12
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 160)
      )
    )
    ;; raybench.cpp:769
    (set_local $15
      (i32.load
        (i32.const 5488)
      )
    )
    ;; raybench.cpp:769
    (set_local $7
      (i32.load
        (get_local $15)
      )
    )
    ;; raybench.cpp:769
    (set_local $7
      (i32.load
        (get_local $7)
      )
    )
    ;; raybench.cpp:769
    (set_local $7
      (call_indirect $FUNCSIG$iiiiddi
        (get_local $15)
        (get_local $1)
        (get_local $2)
        (get_local $3)
        (get_local $4)
        (tee_local $16
          (i32.add
            (get_local $12)
            (i32.const 144)
          )
        )
        (i32.add
          (i32.and
            (get_local $7)
            (i32.const 7)
          )
          (i32.const 28)
        )
      )
    )
    (if
      (i32.eqz
        (get_local $7)
      )
      (block
        (i64.store
          (get_local $0)
          (i64.load
            (i32.const 5440)
          )
        )
        (i64.store offset=8
          (get_local $0)
          (i64.load
            (i32.const 5448)
          )
        )
        ;; raybench.cpp:798
        (i64.store offset=16
          (get_local $0)
          (i64.load
            (i32.const 5456)
          )
        )
        (set_global $STACKTOP
          (get_local $12)
        )
        (return)
      )
    )
    (set_local $15
      (get_local $12)
    )
    ;; raybench.cpp:773
    (set_local $3
      (f64.load
        (get_local $16)
      )
    )
    (set_local $4
      (f64.load
        (tee_local $25
          (i32.add
            (get_local $2)
            (i32.const 8)
          )
        )
      )
    )
    (set_local $8
      (f64.load
        (tee_local $26
          (i32.add
            (get_local $2)
            (i32.const 16)
          )
        )
      )
    )
    ;; raybench.cpp:179
    (set_local $6
      (f64.mul
        (get_local $3)
        (f64.load
          (get_local $2)
        )
      )
    )
    (set_local $10
      (f64.load offset=8
        (get_local $1)
      )
    )
    (set_local $14
      (f64.load offset=16
        (get_local $1)
      )
    )
    ;; raybench.cpp:163
    (set_local $6
      (f64.add
        (get_local $6)
        (f64.load
          (get_local $1)
        )
      )
    )
    ;; raybench.cpp:148
    (f64.store
      (tee_local $16
        (i32.add
          (get_local $12)
          (i32.const 120)
        )
      )
      (get_local $6)
    )
    ;; raybench.cpp:149
    (f64.store
      (tee_local $23
        (i32.add
          (get_local $16)
          (i32.const 8)
        )
      )
      (f64.add
        (f64.mul
          (get_local $3)
          (get_local $4)
        )
        (get_local $10)
      )
    )
    ;; raybench.cpp:150
    (f64.store
      (tee_local $24
        (i32.add
          (get_local $16)
          (i32.const 16)
        )
      )
      (f64.add
        (f64.mul
          (get_local $3)
          (get_local $8)
        )
        (get_local $14)
      )
    )
    ;; raybench.cpp:774
    (set_local $1
      (i32.load
        (get_local $7)
      )
    )
    ;; raybench.cpp:774
    (set_local $1
      (i32.load offset=4
        (get_local $1)
      )
    )
    ;; raybench.cpp:774
    (call_indirect $FUNCSIG$viii
      (tee_local $21
        (i32.add
          (get_local $12)
          (i32.const 96)
        )
      )
      (get_local $7)
      (get_local $16)
      (i32.add
        (i32.and
          (get_local $1)
          (i32.const 15)
        )
        (i32.const 56)
      )
    )
    (set_local $6
      (f64.load
        (i32.const 5472)
      )
    )
    (set_local $10
      (f64.load
        (i32.const 5480)
      )
    )
    (set_local $4
      (f64.load
        (get_local $23)
      )
    )
    (set_local $8
      (f64.load
        (get_local $24)
      )
    )
    ;; raybench.cpp:171
    (set_local $3
      (f64.sub
        (f64.load
          (i32.const 5464)
        )
        (tee_local $14
          (f64.load
            (get_local $16)
          )
        )
      )
    )
    ;; raybench.cpp:148
    (f64.store
      (tee_local $17
        (i32.add
          (get_local $12)
          (i32.const 72)
        )
      )
      (tee_local $20
        (f64.div
          (get_local $3)
          (tee_local $10
            (f64.sqrt
              (f64.add
                (f64.add
                  (f64.mul
                    (get_local $3)
                    (get_local $3)
                  )
                  (f64.mul
                    (tee_local $3
                      (f64.sub
                        (get_local $6)
                        (get_local $4)
                      )
                    )
                    (get_local $3)
                  )
                )
                (f64.mul
                  (tee_local $6
                    (f64.sub
                      (get_local $10)
                      (get_local $8)
                    )
                  )
                  (get_local $6)
                )
              )
            )
          )
        )
      )
    )
    ;; raybench.cpp:149
    (f64.store
      (tee_local $27
        (i32.add
          (get_local $17)
          (i32.const 8)
        )
      )
      (tee_local $3
        (f64.div
          (get_local $3)
          (get_local $10)
        )
      )
    )
    ;; raybench.cpp:150
    (f64.store
      (tee_local $28
        (i32.add
          (get_local $17)
          (i32.const 16)
        )
      )
      (tee_local $6
        (f64.div
          (get_local $6)
          (get_local $10)
        )
      )
    )
    (i64.store
      (get_local $0)
      (i64.load
        (tee_local $1
          (i32.add
            (get_local $7)
            (i32.const 64)
          )
        )
      )
    )
    (i64.store offset=8
      (get_local $0)
      (i64.load offset=8
        (get_local $1)
      )
    )
    ;; raybench.cpp:776
    (i64.store offset=16
      (get_local $0)
      (i64.load offset=16
        (get_local $1)
      )
    )
    ;; raybench.cpp:781
    (set_local $18
      (i32.load
        (i32.const 5488)
      )
    )
    ;; raybench.cpp:781
    (set_local $1
      (i32.load
        (get_local $18)
      )
    )
    ;; raybench.cpp:781
    (set_local $22
      (i32.load
        (get_local $1)
      )
    )
    ;; raybench.cpp:148
    (f64.store
      (tee_local $1
        (i32.add
          (get_local $12)
          (i32.const 24)
        )
      )
      (f64.add
        (f64.mul
          (get_local $20)
          (f64.const 1e-05)
        )
        (get_local $14)
      )
    )
    ;; raybench.cpp:149
    (f64.store offset=8
      (get_local $1)
      (f64.add
        (f64.mul
          (get_local $3)
          (f64.const 1e-05)
        )
        (get_local $4)
      )
    )
    ;; raybench.cpp:150
    (f64.store offset=16
      (get_local $1)
      (f64.add
        (f64.mul
          (get_local $6)
          (f64.const 1e-05)
        )
        (get_local $8)
      )
    )
    ;; raybench.cpp:781
    (set_local $22
      (call_indirect $FUNCSIG$iiiiddi
        (get_local $18)
        (get_local $1)
        (get_local $17)
        (f64.const 1e-05)
        (f64.const 1.e+32)
        (tee_local $18
          (i32.add
            (get_local $12)
            (i32.const 48)
          )
        )
        (i32.add
          (i32.and
            (get_local $22)
            (i32.const 7)
          )
          (i32.const 28)
        )
      )
    )
    (if
      (i32.eqz
        (get_local $22)
      )
      (block
        (set_local $10
          (f64.load
            (get_local $21)
          )
        )
        (set_local $14
          (f64.load offset=8
            (get_local $21)
          )
        )
        (set_local $20
          (f64.load offset=16
            (get_local $21)
          )
        )
        (set_local $9
          (f64.load
            (get_local $17)
          )
        )
        (set_local $11
          (f64.load
            (get_local $27)
          )
        )
        ;; raybench.cpp:208
        (set_local $13
          (f64.load
            (get_local $28)
          )
        )
        ;; raybench.cpp:212
        (if
          (f64.lt
            (tee_local $3
              (f64.add
                (f64.add
                  (f64.mul
                    (get_local $10)
                    (get_local $9)
                  )
                  (f64.mul
                    (get_local $14)
                    (get_local $11)
                  )
                )
                (f64.mul
                  (get_local $20)
                  (get_local $13)
                )
              )
            )
            (f64.const 0)
          )
          (set_local $3
            (f64.const 0)
          )
        )
        (set_local $4
          (f64.load
            (get_local $2)
          )
        )
        (set_local $8
          (f64.load
            (get_local $25)
          )
        )
        ;; raybench.cpp:191
        (set_local $6
          (f64.load
            (get_local $26)
          )
        )
        ;; raybench.cpp:163
        (set_local $9
          (f64.add
            (get_local $9)
            (f64.div
              (f64.neg
                (get_local $4)
              )
              (tee_local $19
                (f64.sqrt
                  (f64.add
                    (f64.add
                      (f64.mul
                        (get_local $4)
                        (get_local $4)
                      )
                      (f64.mul
                        (get_local $8)
                        (get_local $8)
                      )
                    )
                    (f64.mul
                      (get_local $6)
                      (get_local $6)
                    )
                  )
                )
              )
            )
          )
        )
        ;; raybench.cpp:195
        (set_local $11
          (f64.sqrt
            (f64.add
              (f64.mul
                (tee_local $13
                  (f64.add
                    (get_local $13)
                    (f64.div
                      (f64.neg
                        (get_local $6)
                      )
                      (get_local $19)
                    )
                  )
                )
                (get_local $13)
              )
              (f64.add
                (f64.mul
                  (get_local $9)
                  (get_local $9)
                )
                (f64.mul
                  (tee_local $19
                    (f64.add
                      (get_local $11)
                      (f64.div
                        (f64.neg
                          (get_local $8)
                        )
                        (get_local $19)
                      )
                    )
                  )
                  (get_local $19)
                )
              )
            )
          )
        )
        ;; raybench.cpp:787
        (set_local $2
          (i32.load offset=56
            (get_local $7)
          )
        )
        ;; raybench.cpp:787
        (set_local $9
          (call $__ZL3Powdj
            (if f64
              (f64.lt
                (tee_local $9
                  (f64.add
                    (f64.mul
                      (get_local $20)
                      (f64.div
                        (get_local $13)
                        (get_local $11)
                      )
                    )
                    (f64.add
                      (f64.mul
                        (get_local $10)
                        (f64.div
                          (get_local $9)
                          (get_local $11)
                        )
                      )
                      (f64.mul
                        (get_local $14)
                        (f64.div
                          (get_local $19)
                          (get_local $11)
                        )
                      )
                    )
                  )
                )
                (f64.const 0)
              )
              (f64.const 0)
              (get_local $9)
            )
            (get_local $2)
          )
        )
        (set_local $13
          (f64.load offset=16
            (get_local $7)
          )
        )
        (set_local $19
          (f64.load offset=24
            (get_local $7)
          )
        )
        ;; raybench.cpp:179
        (set_local $11
          (f64.mul
            (get_local $3)
            (f64.load offset=8
              (get_local $7)
            )
          )
        )
        (set_local $29
          (f64.load offset=40
            (get_local $7)
          )
        )
        (set_local $30
          (f64.load offset=48
            (get_local $7)
          )
        )
        ;; raybench.cpp:179
        (set_local $31
          (f64.mul
            (get_local $9)
            (f64.load offset=32
              (get_local $7)
            )
          )
        )
        (set_local $32
          (f64.load
            (tee_local $2
              (i32.add
                (get_local $0)
                (i32.const 8)
              )
            )
          )
        )
        (set_local $33
          (f64.load
            (tee_local $17
              (i32.add
                (get_local $0)
                (i32.const 16)
              )
            )
          )
        )
        ;; raybench.cpp:163
        (set_local $11
          (f64.add
            (f64.add
              (get_local $11)
              (get_local $31)
            )
            (f64.load
              (get_local $0)
            )
          )
        )
        ;; raybench.cpp:788
        (f64.store
          (get_local $0)
          (get_local $11)
        )
        ;; raybench.cpp:788
        (f64.store
          (get_local $2)
          (tee_local $13
            (f64.add
              (f64.add
                (f64.mul
                  (get_local $3)
                  (get_local $13)
                )
                (f64.mul
                  (get_local $9)
                  (get_local $29)
                )
              )
              (get_local $32)
            )
          )
        )
        ;; raybench.cpp:788
        (f64.store
          (get_local $17)
          (tee_local $9
            (f64.add
              (f64.add
                (f64.mul
                  (get_local $3)
                  (get_local $19)
                )
                (f64.mul
                  (get_local $9)
                  (get_local $30)
                )
              )
              (get_local $33)
            )
          )
        )
        (if
          ;; raybench.cpp:790
          (get_local $5)
          (block
            ;; raybench.cpp:790
            (set_local $3
              (f64.load
                (tee_local $7
                  (i32.add
                    (get_local $7)
                    (i32.const 88)
                  )
                )
              )
            )
            (if
              ;; raybench.cpp:208
              (f64.ne
                (get_local $3)
                (f64.const 0)
              )
              (block
                ;; raybench.cpp:148
                (f64.store
                  (get_local $18)
                  (tee_local $4
                    (f64.sub
                      (get_local $4)
                      (f64.mul
                        (get_local $10)
                        (tee_local $3
                          (f64.mul
                            (f64.add
                              (f64.add
                                (f64.mul
                                  (get_local $4)
                                  (get_local $10)
                                )
                                (f64.mul
                                  (get_local $8)
                                  (get_local $14)
                                )
                              )
                              (f64.mul
                                (get_local $6)
                                (get_local $20)
                              )
                            )
                            (f64.const 2)
                          )
                        )
                      )
                    )
                  )
                )
                ;; raybench.cpp:149
                (f64.store offset=8
                  (get_local $18)
                  (tee_local $8
                    (f64.sub
                      (get_local $8)
                      (f64.mul
                        (get_local $14)
                        (get_local $3)
                      )
                    )
                  )
                )
                ;; raybench.cpp:150
                (f64.store offset=16
                  (get_local $18)
                  (tee_local $3
                    (f64.sub
                      (get_local $6)
                      (f64.mul
                        (get_local $20)
                        (get_local $3)
                      )
                    )
                  )
                )
                (set_local $6
                  (f64.load
                    (get_local $23)
                  )
                )
                (set_local $10
                  (f64.load
                    (get_local $24)
                  )
                )
                ;; raybench.cpp:163
                (set_local $4
                  (f64.add
                    (f64.mul
                      (get_local $4)
                      (f64.const 1e-05)
                    )
                    (f64.load
                      (get_local $16)
                    )
                  )
                )
                ;; raybench.cpp:148
                (f64.store
                  (get_local $15)
                  (get_local $4)
                )
                ;; raybench.cpp:149
                (f64.store offset=8
                  (get_local $15)
                  (f64.add
                    (f64.mul
                      (get_local $8)
                      (f64.const 1e-05)
                    )
                    (get_local $6)
                  )
                )
                ;; raybench.cpp:150
                (f64.store offset=16
                  (get_local $15)
                  (f64.add
                    (f64.mul
                      (get_local $3)
                      (f64.const 1e-05)
                    )
                    (get_local $10)
                  )
                )
                ;; raybench.cpp:792
                (call $__Z8raycolorRK4Vec3S1_ddj
                  (get_local $1)
                  (get_local $15)
                  (get_local $18)
                  (f64.const 1e-05)
                  (f64.const 1.e+32)
                  (i32.add
                    (get_local $5)
                    (i32.const -1)
                  )
                )
                ;; raybench.cpp:792
                (set_local $3
                  (f64.load
                    (get_local $7)
                  )
                )
                (set_local $4
                  (f64.load offset=8
                    (get_local $1)
                  )
                )
                (set_local $8
                  (f64.load offset=16
                    (get_local $1)
                  )
                )
                ;; raybench.cpp:179
                (set_local $6
                  (f64.mul
                    (get_local $3)
                    (f64.load
                      (get_local $1)
                    )
                  )
                )
                ;; raybench.cpp:792
                (f64.store
                  (get_local $0)
                  (f64.add
                    (get_local $6)
                    (get_local $11)
                  )
                )
                ;; raybench.cpp:792
                (f64.store
                  (get_local $2)
                  (f64.add
                    (f64.mul
                      (get_local $3)
                      (get_local $4)
                    )
                    (get_local $13)
                  )
                )
                ;; raybench.cpp:792
                (f64.store
                  (get_local $17)
                  (f64.add
                    (f64.mul
                      (get_local $3)
                      (get_local $8)
                    )
                    (get_local $9)
                  )
                )
              )
            )
          )
        )
      )
    )
    (set_global $STACKTOP
      (get_local $12)
    )
  )
  (func $__ZL3Powdj (param $0 f64) (param $1 i32) (result f64)
    (local $2 f64)
    ;; raybench.cpp:57
    (if
      ;; raybench.cpp:64
      (i32.le_u
        (get_local $1)
        (i32.const 1)
      )
      (return
        (if f64
          (i32.eq
            (get_local $1)
            (i32.const 1)
          )
          (get_local $0)
          (f64.const 1)
        )
      )
    )
    (if f64
      ;; raybench.cpp:58
      (i32.and
        (get_local $1)
        (i32.const 1)
      )
      ;; raybench.cpp:60
      (block f64
        ;; raybench.cpp:58
        (set_local $2
          (call $__ZL3Powdj
            (get_local $0)
            (i32.add
              (get_local $1)
              (i32.const -1)
            )
          )
        )
        (f64.mul
          (get_local $2)
          (get_local $0)
        )
      )
      (block f64
        ;; raybench.cpp:60
        (set_local $0
          (call $__ZL3Powdj
            (get_local $0)
            (i32.shr_u
              (get_local $1)
              (i32.const 1)
            )
          )
        )
        (f64.mul
          (get_local $0)
          (get_local $0)
        )
      )
    )
  )
  (func $__Z9rectangleRNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEEERK8MaterialRK4Vec3SC_SC_SC_ (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32) (param $5 i32)
    (local $6 i32)
    (local $7 i32)
    (local $8 f64)
    (local $9 i32)
    (local $10 f64)
    (local $11 f64)
    (local $12 f64)
    (local $13 f64)
    (local $14 f64)
    (local $15 f64)
    (local $16 f64)
    (local $17 f64)
    (local $18 i32)
    (local $19 i32)
    (local $20 i32)
    (local $21 i32)
    (local $22 i32)
    (local $23 f64)
    (local $24 f64)
    (local $25 f64)
    (local $26 f64)
    (local $27 i32)
    (local $28 i32)
    (set_local $19
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 16)
      )
    )
    ;; raybench.cpp:815
    (set_local $7
      (call $__Znwj
        (i32.const 192)
      )
    )
    (i64.store
      (tee_local $6
        (i32.add
          (get_local $7)
          (i32.const 8)
        )
      )
      (i64.load
        (get_local $1)
      )
    )
    (i64.store offset=8
      (get_local $6)
      (i64.load offset=8
        (get_local $1)
      )
    )
    (i64.store offset=16
      (get_local $6)
      (i64.load offset=16
        (get_local $1)
      )
    )
    (i64.store offset=24
      (get_local $6)
      (i64.load offset=24
        (get_local $1)
      )
    )
    (i64.store offset=32
      (get_local $6)
      (i64.load offset=32
        (get_local $1)
      )
    )
    (i64.store offset=40
      (get_local $6)
      (i64.load offset=40
        (get_local $1)
      )
    )
    (i64.store offset=48
      (get_local $6)
      (i64.load offset=48
        (get_local $1)
      )
    )
    (i64.store offset=56
      (get_local $6)
      (i64.load offset=56
        (get_local $1)
      )
    )
    (i64.store offset=64
      (get_local $6)
      (i64.load offset=64
        (get_local $1)
      )
    )
    (i64.store offset=72
      (get_local $6)
      (i64.load offset=72
        (get_local $1)
      )
    )
    ;; raybench.cpp:277
    (i64.store offset=80
      (get_local $6)
      (i64.load offset=80
        (get_local $1)
      )
    )
    ;; raybench.cpp:511
    (i32.store
      (get_local $7)
      (i32.const 1512)
    )
    (i64.store
      (tee_local $6
        (i32.add
          (get_local $7)
          (i32.const 96)
        )
      )
      (i64.load
        (get_local $2)
      )
    )
    (i64.store offset=8
      (get_local $6)
      (i64.load offset=8
        (get_local $2)
      )
    )
    ;; raybench.cpp:507
    (i64.store offset=16
      (get_local $6)
      (i64.load offset=16
        (get_local $2)
      )
    )
    (i64.store
      (tee_local $6
        (i32.add
          (get_local $7)
          (i32.const 120)
        )
      )
      (i64.load
        (get_local $3)
      )
    )
    (i64.store offset=8
      (get_local $6)
      (i64.load offset=8
        (get_local $3)
      )
    )
    ;; raybench.cpp:508
    (i64.store offset=16
      (get_local $6)
      (i64.load offset=16
        (get_local $3)
      )
    )
    (i64.store
      (tee_local $6
        (i32.add
          (get_local $7)
          (i32.const 144)
        )
      )
      (i64.load
        (get_local $4)
      )
    )
    (i64.store offset=8
      (get_local $6)
      (i64.load offset=8
        (get_local $4)
      )
    )
    ;; raybench.cpp:509
    (i64.store offset=16
      (get_local $6)
      (i64.load offset=16
        (get_local $4)
      )
    )
    ;; raybench.cpp:815
    (set_local $14
      (f64.load
        (get_local $3)
      )
    )
    ;; raybench.cpp:815
    (set_local $12
      (f64.load offset=8
        (get_local $3)
      )
    )
    ;; raybench.cpp:815
    (set_local $8
      (f64.load offset=16
        (get_local $3)
      )
    )
    ;; raybench.cpp:815
    (set_local $13
      (f64.load
        (get_local $2)
      )
    )
    ;; raybench.cpp:815
    (set_local $10
      (f64.load
        (tee_local $9
          (i32.add
            (get_local $2)
            (i32.const 8)
          )
        )
      )
    )
    ;; raybench.cpp:815
    (set_local $11
      (f64.load
        (tee_local $20
          (i32.add
            (get_local $2)
            (i32.const 16)
          )
        )
      )
    )
    ;; raybench.cpp:815
    (set_local $15
      (f64.load
        (get_local $4)
      )
    )
    ;; raybench.cpp:815
    (set_local $16
      (f64.load
        (tee_local $21
          (i32.add
            (get_local $4)
            (i32.const 8)
          )
        )
      )
    )
    ;; raybench.cpp:815
    (set_local $17
      (f64.load
        (tee_local $22
          (i32.add
            (get_local $4)
            (i32.const 16)
          )
        )
      )
    )
    ;; raybench.cpp:204
    (set_local $12
      (f64.sub
        (f64.mul
          (tee_local $23
            (f64.sub
              (get_local $12)
              (get_local $10)
            )
          )
          (tee_local $24
            (f64.sub
              (get_local $17)
              (get_local $11)
            )
          )
        )
        (f64.mul
          (tee_local $8
            (f64.sub
              (get_local $8)
              (get_local $11)
            )
          )
          (tee_local $25
            (f64.sub
              (get_local $16)
              (get_local $10)
            )
          )
        )
      )
    )
    ;; raybench.cpp:204
    (set_local $14
      (f64.sub
        (f64.mul
          (get_local $8)
          (tee_local $8
            (f64.sub
              (get_local $15)
              (get_local $13)
            )
          )
        )
        (f64.mul
          (tee_local $26
            (f64.sub
              (get_local $14)
              (get_local $13)
            )
          )
          (get_local $24)
        )
      )
    )
    ;; raybench.cpp:148
    (f64.store offset=168
      (get_local $7)
      (f64.div
        (get_local $12)
        (tee_local $12
          (f64.sqrt
            (f64.add
              (f64.mul
                (tee_local $8
                  (f64.sub
                    (f64.mul
                      (get_local $26)
                      (get_local $25)
                    )
                    (f64.mul
                      (get_local $23)
                      (get_local $8)
                    )
                  )
                )
                (get_local $8)
              )
              (f64.add
                (f64.mul
                  (get_local $12)
                  (get_local $12)
                )
                (f64.mul
                  (get_local $14)
                  (get_local $14)
                )
              )
            )
          )
        )
      )
    )
    ;; raybench.cpp:149
    (f64.store offset=176
      (get_local $7)
      (f64.div
        (get_local $14)
        (get_local $12)
      )
    )
    ;; raybench.cpp:150
    (f64.store offset=184
      (get_local $7)
      (f64.div
        (get_local $8)
        (get_local $12)
      )
    )
    ;; raybench.cpp:815
    (i32.store
      (tee_local $6
        (get_local $19)
      )
      (get_local $7)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1604
    (set_local $3
      (i32.load
        (tee_local $18
          (i32.add
            (get_local $0)
            (i32.const 4)
          )
        )
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1604
    (set_local $28
      (i32.load
        (tee_local $27
          (i32.add
            (get_local $0)
            (i32.const 8)
          )
        )
      )
    )
    (if
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
      (i32.lt_u
        (get_local $3)
        (get_local $28)
      )
      (block
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
        (i32.store
          (get_local $3)
          (get_local $7)
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1611
        (set_local $3
          (i32.load
            (get_local $18)
          )
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1611
        (i32.store
          (get_local $18)
          (tee_local $3
            (i32.add
              (get_local $3)
              (i32.const 4)
            )
          )
        )
      )
      (block
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1614
        (call $__ZNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEE21__push_back_slow_pathIS2_EEvOT_
          (get_local $0)
          (get_local $6)
        )
        ;; raybench.cpp:816
        (set_local $15
          (f64.load
            (get_local $4)
          )
        )
        ;; raybench.cpp:816
        (set_local $16
          (f64.load
            (get_local $21)
          )
        )
        ;; raybench.cpp:816
        (set_local $17
          (f64.load
            (get_local $22)
          )
        )
        ;; raybench.cpp:816
        (set_local $13
          (f64.load
            (get_local $2)
          )
        )
        ;; raybench.cpp:816
        (set_local $10
          (f64.load
            (get_local $9)
          )
        )
        ;; raybench.cpp:816
        (set_local $11
          (f64.load
            (get_local $20)
          )
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1604
        (set_local $3
          (i32.load
            (get_local $18)
          )
        )
      )
    )
    ;; raybench.cpp:816
    (set_local $7
      (call $__Znwj
        (i32.const 192)
      )
    )
    (i64.store
      (tee_local $9
        (i32.add
          (get_local $7)
          (i32.const 8)
        )
      )
      (i64.load
        (get_local $1)
      )
    )
    (i64.store offset=8
      (get_local $9)
      (i64.load offset=8
        (get_local $1)
      )
    )
    (i64.store offset=16
      (get_local $9)
      (i64.load offset=16
        (get_local $1)
      )
    )
    (i64.store offset=24
      (get_local $9)
      (i64.load offset=24
        (get_local $1)
      )
    )
    (i64.store offset=32
      (get_local $9)
      (i64.load offset=32
        (get_local $1)
      )
    )
    (i64.store offset=40
      (get_local $9)
      (i64.load offset=40
        (get_local $1)
      )
    )
    (i64.store offset=48
      (get_local $9)
      (i64.load offset=48
        (get_local $1)
      )
    )
    (i64.store offset=56
      (get_local $9)
      (i64.load offset=56
        (get_local $1)
      )
    )
    (i64.store offset=64
      (get_local $9)
      (i64.load offset=64
        (get_local $1)
      )
    )
    (i64.store offset=72
      (get_local $9)
      (i64.load offset=72
        (get_local $1)
      )
    )
    ;; raybench.cpp:277
    (i64.store offset=80
      (get_local $9)
      (i64.load offset=80
        (get_local $1)
      )
    )
    ;; raybench.cpp:511
    (i32.store
      (get_local $7)
      (i32.const 1512)
    )
    (i64.store
      (tee_local $1
        (i32.add
          (get_local $7)
          (i32.const 96)
        )
      )
      (i64.load
        (get_local $2)
      )
    )
    (i64.store offset=8
      (get_local $1)
      (i64.load offset=8
        (get_local $2)
      )
    )
    ;; raybench.cpp:507
    (i64.store offset=16
      (get_local $1)
      (i64.load offset=16
        (get_local $2)
      )
    )
    (i64.store
      (tee_local $1
        (i32.add
          (get_local $7)
          (i32.const 120)
        )
      )
      (i64.load
        (get_local $4)
      )
    )
    (i64.store offset=8
      (get_local $1)
      (i64.load offset=8
        (get_local $4)
      )
    )
    ;; raybench.cpp:508
    (i64.store offset=16
      (get_local $1)
      (i64.load offset=16
        (get_local $4)
      )
    )
    (i64.store
      (tee_local $1
        (i32.add
          (get_local $7)
          (i32.const 144)
        )
      )
      (i64.load
        (get_local $5)
      )
    )
    (i64.store offset=8
      (get_local $1)
      (i64.load offset=8
        (get_local $5)
      )
    )
    ;; raybench.cpp:509
    (i64.store offset=16
      (get_local $1)
      (i64.load offset=16
        (get_local $5)
      )
    )
    ;; raybench.cpp:816
    (set_local $12
      (f64.load
        (get_local $5)
      )
    )
    ;; raybench.cpp:816
    (set_local $14
      (f64.load offset=8
        (get_local $5)
      )
    )
    ;; raybench.cpp:816
    (set_local $8
      (f64.load offset=16
        (get_local $5)
      )
    )
    ;; raybench.cpp:204
    (set_local $10
      (f64.sub
        (f64.mul
          (tee_local $16
            (f64.sub
              (get_local $16)
              (get_local $10)
            )
          )
          (tee_local $8
            (f64.sub
              (get_local $8)
              (get_local $11)
            )
          )
        )
        (f64.mul
          (tee_local $11
            (f64.sub
              (get_local $17)
              (get_local $11)
            )
          )
          (tee_local $17
            (f64.sub
              (get_local $14)
              (get_local $10)
            )
          )
        )
      )
    )
    ;; raybench.cpp:204
    (set_local $13
      (f64.sub
        (f64.mul
          (get_local $11)
          (tee_local $11
            (f64.sub
              (get_local $12)
              (get_local $13)
            )
          )
        )
        (f64.mul
          (tee_local $15
            (f64.sub
              (get_local $15)
              (get_local $13)
            )
          )
          (get_local $8)
        )
      )
    )
    ;; raybench.cpp:148
    (f64.store offset=168
      (get_local $7)
      (f64.div
        (get_local $10)
        (tee_local $10
          (f64.sqrt
            (f64.add
              (f64.mul
                (tee_local $11
                  (f64.sub
                    (f64.mul
                      (get_local $15)
                      (get_local $17)
                    )
                    (f64.mul
                      (get_local $16)
                      (get_local $11)
                    )
                  )
                )
                (get_local $11)
              )
              (f64.add
                (f64.mul
                  (get_local $10)
                  (get_local $10)
                )
                (f64.mul
                  (get_local $13)
                  (get_local $13)
                )
              )
            )
          )
        )
      )
    )
    ;; raybench.cpp:149
    (f64.store offset=176
      (get_local $7)
      (f64.div
        (get_local $13)
        (get_local $10)
      )
    )
    ;; raybench.cpp:150
    (f64.store offset=184
      (get_local $7)
      (f64.div
        (get_local $11)
        (get_local $10)
      )
    )
    ;; raybench.cpp:816
    (i32.store
      (get_local $6)
      (get_local $7)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1604
    (set_local $1
      (i32.load
        (get_local $27)
      )
    )
    (if
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
      (i32.lt_u
        (get_local $3)
        (get_local $1)
      )
      (block
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
        (i32.store
          (get_local $3)
          (get_local $7)
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1611
        (set_local $0
          (i32.load
            (get_local $18)
          )
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1611
        (i32.store
          (get_local $18)
          (i32.add
            (get_local $0)
            (i32.const 4)
          )
        )
        (set_global $STACKTOP
          (get_local $19)
        )
      )
      (block
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1614
        (call $__ZNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEE21__push_back_slow_pathIS2_EEvOT_
          (get_local $0)
          (get_local $6)
        )
        (set_global $STACKTOP
          (get_local $19)
        )
      )
    )
  )
  (func $__Z9partitionRNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEEE6Boundsj (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
    (local $3 i32)
    (local $4 i32)
    (local $5 i32)
    (local $6 i32)
    (local $7 i32)
    (local $8 f64)
    (local $9 f64)
    (local $10 i32)
    (local $11 f64)
    (local $12 f64)
    (local $13 i32)
    (local $14 i32)
    (local $15 i32)
    (local $16 i32)
    (local $17 i32)
    (local $18 i32)
    (local $19 i32)
    (local $20 f64)
    (local $21 f64)
    (local $22 f64)
    (local $23 i32)
    (local $24 i32)
    (local $25 i32)
    (local $26 i32)
    (local $27 i32)
    (local $28 i32)
    (local $29 i32)
    (local $30 i32)
    (local $31 i32)
    (local $32 i32)
    (local $33 i32)
    (local $34 i32)
    (local $35 i32)
    (local $36 i32)
    (local $37 i32)
    (local $38 i32)
    (local $39 i32)
    (local $40 i32)
    (local $41 i32)
    (set_local $14
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 304)
      )
    )
    (set_local $3
      (i32.add
        (get_local $14)
        (i32.const 176)
      )
    )
    (set_local $26
      (i32.add
        (get_local $14)
        (i32.const 168)
      )
    )
    (set_local $27
      (i32.add
        (get_local $14)
        (i32.const 292)
      )
    )
    (set_local $28
      (i32.add
        (get_local $14)
        (i32.const 288)
      )
    )
    (set_local $18
      (i32.add
        (get_local $14)
        (i32.const 276)
      )
    )
    (set_local $19
      (i32.add
        (get_local $14)
        (i32.const 264)
      )
    )
    (set_local $29
      (i32.add
        (get_local $14)
        (i32.const 144)
      )
    )
    (set_local $30
      (i32.add
        (get_local $14)
        (i32.const 120)
      )
    )
    (set_local $31
      (i32.add
        (get_local $14)
        (i32.const 96)
      )
    )
    (set_local $10
      (i32.add
        (get_local $14)
        (i32.const 48)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:640
    (set_local $15
      (get_local $14)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:640
    (set_local $5
      (i32.load
        (tee_local $32
          (i32.add
            (get_local $0)
            (i32.const 4)
          )
        )
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:640
    (set_local $4
      (i32.load
        (get_local $0)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1499
    (set_local $6
      (get_local $4)
    )
    (block $switch
      (block $switch-default19
        (block $switch-case0
          (block $switch-case
            (br_table $switch-case $switch-case0 $switch-default19
              (i32.sub
                (i32.shr_s
                  (i32.sub
                    (get_local $5)
                    (get_local $4)
                  )
                  (i32.const 2)
                )
                (i32.const 1)
              )
            )
          )
          ;; raybench.cpp:859
          (set_local $0
            (i32.load
              (get_local $6)
            )
          )
          (set_local $2
            (i32.const 0)
          )
          (br $switch)
        )
        ;; raybench.cpp:863
        (set_local $0
          (i32.load
            (get_local $6)
          )
        )
        ;; raybench.cpp:864
        (set_local $2
          (i32.load offset=4
            (get_local $6)
          )
        )
        (br $switch)
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:432
      (i32.store
        (get_local $18)
        (i32.const 0)
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:433
      (i32.store
        (tee_local $16
          (i32.add
            (get_local $18)
            (i32.const 4)
          )
        )
        (i32.const 0)
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:2252
      (i32.store offset=8
        (get_local $18)
        (i32.const 0)
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:432
      (i32.store
        (get_local $19)
        (i32.const 0)
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:433
      (i32.store
        (tee_local $17
          (i32.add
            (get_local $19)
            (i32.const 4)
          )
        )
        (i32.const 0)
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:2252
      (i32.store offset=8
        (get_local $19)
        (i32.const 0)
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:2312
      (set_local $33
        (i32.add
          (get_local $19)
          (i32.const 8)
        )
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:2312
      (set_local $34
        (i32.add
          (get_local $18)
          (i32.const 8)
        )
      )
      ;; raybench.cpp:884
      (set_local $35
        (i32.add
          (get_local $1)
          (i32.const 8)
        )
      )
      ;; raybench.cpp:888
      (set_local $36
        (i32.add
          (get_local $1)
          (i32.const 24)
        )
      )
      ;; raybench.cpp:888
      (set_local $37
        (i32.add
          (get_local $1)
          (i32.const 16)
        )
      )
      ;; raybench.cpp:889
      (set_local $38
        (i32.add
          (get_local $30)
          (i32.const 8)
        )
      )
      ;; raybench.cpp:892
      (set_local $39
        (i32.add
          (get_local $1)
          (i32.const 40)
        )
      )
      ;; raybench.cpp:892
      (set_local $40
        (i32.add
          (get_local $1)
          (i32.const 32)
        )
      )
      ;; raybench.cpp:893
      (set_local $41
        (i32.add
          (get_local $31)
          (i32.const 16)
        )
      )
      (set_local $4
        (get_local $2)
      )
      (set_local $13
        (i32.const 3)
      )
      (set_local $2
        (get_local $6)
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/iterator:1366
      (set_local $6
        (get_local $5)
      )
      (block $do-once15
        (block $__rjti$3
          (block $__rjti$2
            (block $__rjti$1
              (block $__rjti$0
                (loop $label$continue$L3
                  ;; raybench.cpp:901
                  (block $label$break$L5
                    (if
                      (i32.ne
                        (get_local $2)
                        (get_local $6)
                      )
                      (block
                        (if
                          (get_local $4)
                          (block
                            (set_local $11
                              (f64.const 0)
                            )
                            (set_local $9
                              (f64.const 0)
                            )
                          )
                          (loop $while-in
                            ;; raybench.cpp:881
                            (set_local $5
                              (i32.load
                                (get_local $2)
                              )
                            )
                            ;; raybench.cpp:881
                            (i32.store
                              (get_local $3)
                              (get_local $5)
                            )
                            ;; raybench.cpp:884
                            (set_local $11
                              (f64.load
                                (get_local $35)
                              )
                            )
                            ;; raybench.cpp:884
                            (set_local $9
                              (f64.load
                                (get_local $1)
                              )
                            )
                            ;; raybench.cpp:885
                            (set_local $7
                              (i32.load
                                (get_local $5)
                              )
                            )
                            ;; raybench.cpp:885
                            (set_local $7
                              (i32.load offset=12
                                (get_local $7)
                              )
                            )
                            ;; raybench.cpp:885
                            (call_indirect $FUNCSIG$vii
                              (get_local $29)
                              (get_local $5)
                              (i32.add
                                (i32.and
                                  (get_local $7)
                                  (i32.const 15)
                                )
                                (i32.const 36)
                              )
                            )
                            ;; raybench.cpp:885
                            (set_local $12
                              (f64.load
                                (get_local $29)
                              )
                            )
                            ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/iterator:1198
                            (if
                              (f64.le
                                (get_local $12)
                                (f64.mul
                                  (f64.add
                                    (get_local $11)
                                    (get_local $9)
                                  )
                                  (f64.const 0.5)
                                )
                              )
                              (block
                                ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1585
                                (set_local $5
                                  (i32.load
                                    (get_local $16)
                                  )
                                )
                                ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1585
                                (set_local $7
                                  (i32.load
                                    (get_local $34)
                                  )
                                )
                                (if
                                  (i32.eq
                                    (get_local $5)
                                    (get_local $7)
                                  )
                                  ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1594
                                  (call $__ZNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEE21__push_back_slow_pathIS2_EEvOT_
                                    (get_local $18)
                                    (get_local $3)
                                  )
                                  (block
                                    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
                                    (set_local $7
                                      (i32.load
                                        (get_local $3)
                                      )
                                    )
                                    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
                                    (i32.store
                                      (get_local $5)
                                      (get_local $7)
                                    )
                                    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1591
                                    (set_local $5
                                      (i32.load
                                        (get_local $16)
                                      )
                                    )
                                    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1591
                                    (i32.store
                                      (get_local $16)
                                      (i32.add
                                        (get_local $5)
                                        (i32.const 4)
                                      )
                                    )
                                  )
                                )
                              )
                              (block
                                ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1585
                                (set_local $5
                                  (i32.load
                                    (get_local $17)
                                  )
                                )
                                ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1585
                                (set_local $7
                                  (i32.load
                                    (get_local $33)
                                  )
                                )
                                (if
                                  (i32.eq
                                    (get_local $5)
                                    (get_local $7)
                                  )
                                  ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1594
                                  (call $__ZNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEE21__push_back_slow_pathIS2_EEvOT_
                                    (get_local $19)
                                    (get_local $3)
                                  )
                                  (block
                                    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
                                    (set_local $7
                                      (i32.load
                                        (get_local $3)
                                      )
                                    )
                                    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
                                    (i32.store
                                      (get_local $5)
                                      (get_local $7)
                                    )
                                    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1591
                                    (set_local $5
                                      (i32.load
                                        (get_local $17)
                                      )
                                    )
                                    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1591
                                    (i32.store
                                      (get_local $17)
                                      (i32.add
                                        (get_local $5)
                                        (i32.const 4)
                                      )
                                    )
                                  )
                                )
                              )
                            )
                            (br_if $while-in
                              (i32.ne
                                (tee_local $2
                                  (i32.add
                                    (get_local $2)
                                    (i32.const 4)
                                  )
                                )
                                (get_local $6)
                              )
                            )
                            (br $label$break$L5)
                          )
                        )
                        (loop $while-in3
                          ;; raybench.cpp:881
                          (set_local $5
                            (i32.load
                              (get_local $2)
                            )
                          )
                          ;; raybench.cpp:881
                          (i32.store
                            (get_local $3)
                            (get_local $5)
                          )
                          ;; raybench.cpp:896
                          (block $switch4
                            (block $switch-default
                              (block $switch-case6
                                (block $switch-case5
                                  (br_table $switch-case6 $switch-case5 $switch-default
                                    (i32.sub
                                      (get_local $4)
                                      (i32.const 1)
                                    )
                                  )
                                )
                                ;; raybench.cpp:892
                                (set_local $9
                                  (f64.load
                                    (get_local $39)
                                  )
                                )
                                ;; raybench.cpp:892
                                (set_local $12
                                  (f64.load
                                    (get_local $40)
                                  )
                                )
                                ;; raybench.cpp:893
                                (set_local $7
                                  (i32.load
                                    (get_local $5)
                                  )
                                )
                                ;; raybench.cpp:893
                                (set_local $7
                                  (i32.load offset=12
                                    (get_local $7)
                                  )
                                )
                                ;; raybench.cpp:893
                                (call_indirect $FUNCSIG$vii
                                  (get_local $31)
                                  (get_local $5)
                                  (i32.add
                                    (i32.and
                                      (get_local $7)
                                      (i32.const 15)
                                    )
                                    (i32.const 36)
                                  )
                                )
                                ;; raybench.cpp:893
                                (set_local $11
                                  (f64.load
                                    (get_local $41)
                                  )
                                )
                                (set_local $9
                                  (f64.mul
                                    (f64.add
                                      (get_local $9)
                                      (get_local $12)
                                    )
                                    (f64.const 0.5)
                                  )
                                )
                                (br $switch4)
                              )
                              ;; raybench.cpp:888
                              (set_local $9
                                (f64.load
                                  (get_local $36)
                                )
                              )
                              ;; raybench.cpp:888
                              (set_local $12
                                (f64.load
                                  (get_local $37)
                                )
                              )
                              ;; raybench.cpp:889
                              (set_local $7
                                (i32.load
                                  (get_local $5)
                                )
                              )
                              ;; raybench.cpp:889
                              (set_local $7
                                (i32.load offset=12
                                  (get_local $7)
                                )
                              )
                              ;; raybench.cpp:889
                              (call_indirect $FUNCSIG$vii
                                (get_local $30)
                                (get_local $5)
                                (i32.add
                                  (i32.and
                                    (get_local $7)
                                    (i32.const 15)
                                  )
                                  (i32.const 36)
                                )
                              )
                              ;; raybench.cpp:889
                              (set_local $11
                                (f64.load
                                  (get_local $38)
                                )
                              )
                              (set_local $9
                                (f64.mul
                                  (f64.add
                                    (get_local $9)
                                    (get_local $12)
                                  )
                                  (f64.const 0.5)
                                )
                              )
                            )
                          )
                          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/iterator:1198
                          (if
                            (f64.le
                              (get_local $11)
                              (get_local $9)
                            )
                            (block
                              ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1585
                              (set_local $5
                                (i32.load
                                  (get_local $16)
                                )
                              )
                              ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1585
                              (set_local $7
                                (i32.load
                                  (get_local $34)
                                )
                              )
                              (if
                                (i32.eq
                                  (get_local $5)
                                  (get_local $7)
                                )
                                ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1594
                                (call $__ZNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEE21__push_back_slow_pathIS2_EEvOT_
                                  (get_local $18)
                                  (get_local $3)
                                )
                                (block
                                  ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
                                  (set_local $7
                                    (i32.load
                                      (get_local $3)
                                    )
                                  )
                                  ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
                                  (i32.store
                                    (get_local $5)
                                    (get_local $7)
                                  )
                                  ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1591
                                  (set_local $5
                                    (i32.load
                                      (get_local $16)
                                    )
                                  )
                                  ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1591
                                  (i32.store
                                    (get_local $16)
                                    (i32.add
                                      (get_local $5)
                                      (i32.const 4)
                                    )
                                  )
                                )
                              )
                            )
                            (block
                              ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1585
                              (set_local $5
                                (i32.load
                                  (get_local $17)
                                )
                              )
                              ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1585
                              (set_local $7
                                (i32.load
                                  (get_local $33)
                                )
                              )
                              (if
                                (i32.eq
                                  (get_local $5)
                                  (get_local $7)
                                )
                                ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1594
                                (call $__ZNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEE21__push_back_slow_pathIS2_EEvOT_
                                  (get_local $19)
                                  (get_local $3)
                                )
                                (block
                                  ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
                                  (set_local $7
                                    (i32.load
                                      (get_local $3)
                                    )
                                  )
                                  ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
                                  (i32.store
                                    (get_local $5)
                                    (get_local $7)
                                  )
                                  ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1591
                                  (set_local $5
                                    (i32.load
                                      (get_local $17)
                                    )
                                  )
                                  ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1591
                                  (i32.store
                                    (get_local $17)
                                    (i32.add
                                      (get_local $5)
                                      (i32.const 4)
                                    )
                                  )
                                )
                              )
                            )
                          )
                          (br_if $while-in3
                            (i32.ne
                              (tee_local $2
                                (i32.add
                                  (get_local $2)
                                  (i32.const 4)
                                )
                              )
                              (get_local $6)
                            )
                          )
                          (br $label$break$L5)
                        )
                      )
                    )
                  )
                  ;; raybench.cpp:901
                  (set_local $7
                    (call $i32u-rem
                      (i32.add
                        (get_local $4)
                        (i32.const 1)
                      )
                      (i32.const 3)
                    )
                  )
                  ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:640
                  (set_local $6
                    (i32.load
                      (get_local $17)
                    )
                  )
                  ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:640
                  (set_local $23
                    (i32.load
                      (get_local $19)
                    )
                  )
                  ;; raybench.cpp:902
                  (set_local $4
                    (get_local $6)
                  )
                  ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:424
                  (if
                    (i32.eq
                      (get_local $6)
                      (get_local $23)
                    )
                    (block
                      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:369
                      (set_local $2
                        (i32.load
                          (get_local $18)
                        )
                      )
                      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:424
                      (set_local $6
                        (i32.load
                          (get_local $16)
                        )
                      )
                    )
                    (block
                      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:640
                      (set_local $25
                        (i32.load
                          (get_local $16)
                        )
                      )
                      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:640
                      (set_local $24
                        (i32.load
                          (get_local $18)
                        )
                      )
                      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1499
                      (set_local $2
                        (get_local $24)
                      )
                      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1482
                      (set_local $5
                        (get_local $25)
                      )
                      (block $switch9
                        (block $switch-default12
                          (block $switch-case11
                            (block $switch-case10
                              (br_table $switch-case11 $switch-case10 $switch-default12
                                (i32.sub
                                  (i32.shr_s
                                    (i32.sub
                                      (get_local $25)
                                      (get_local $24)
                                    )
                                    (i32.const 2)
                                  )
                                  (i32.const 0)
                                )
                              )
                            )
                            (br $__rjti$1)
                          )
                          (set_local $6
                            (get_local $5)
                          )
                          (br $switch9)
                        )
                        (br $__rjti$2)
                      )
                    )
                  )
                  ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:424
                  (if
                    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:369
                    (i32.ne
                      (get_local $6)
                      (get_local $2)
                    )
                    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:425
                    (i32.store
                      (get_local $16)
                      (i32.add
                        (get_local $6)
                        (i32.shl
                          (i32.xor
                            (i32.shr_u
                              (i32.sub
                                (i32.add
                                  (get_local $6)
                                  (i32.const -4)
                                )
                                (get_local $2)
                              )
                              (i32.const 2)
                            )
                            (i32.const -1)
                          )
                          (i32.const 2)
                        )
                      )
                    )
                  )
                  ;; raybench.cpp:875
                  (if
                    (i32.ne
                      (get_local $4)
                      (get_local $23)
                    )
                    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:425
                    (i32.store
                      (get_local $17)
                      (i32.add
                        (get_local $4)
                        (i32.shl
                          (i32.xor
                            (i32.shr_u
                              (i32.sub
                                (i32.add
                                  (get_local $4)
                                  (i32.const -4)
                                )
                                (get_local $23)
                              )
                              (i32.const 2)
                            )
                            (i32.const -1)
                          )
                          (i32.const 2)
                        )
                      )
                    )
                  )
                  (br_if $__rjti$0
                    (i32.eqz
                      (tee_local $13
                        (i32.add
                          (get_local $13)
                          (i32.const -1)
                        )
                      )
                    )
                  )
                  ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1466
                  (set_local $2
                    (i32.load
                      (get_local $0)
                    )
                  )
                  ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1482
                  (set_local $6
                    (i32.load
                      (get_local $32)
                    )
                  )
                  (set_local $4
                    (get_local $7)
                  )
                  (br $label$continue$L3)
                )
              )
              ;; raybench.cpp:126
              (set_local $2
                (i32.load
                  (i32.const 1616)
                )
              )
              ;; raybench.cpp:126
              (i32.store
                (get_local $26)
                (i32.const 2403)
              )
              ;; raybench.cpp:126
              (drop
                (call $_fprintf
                  (get_local $2)
                  (i32.const 2351)
                  (get_local $26)
                )
              )
              ;; raybench.cpp:877
              (set_local $2
                (call $__Znwj
                  (i32.const 112)
                )
              )
              (i64.store
                (get_local $3)
                (i64.const 0)
              )
              (i64.store offset=8
                (get_local $3)
                (i64.const 0)
              )
              (i64.store offset=16
                (get_local $3)
                (i64.const 0)
              )
              (i64.store offset=24
                (get_local $3)
                (i64.const 0)
              )
              (i64.store offset=32
                (get_local $3)
                (i64.const 0)
              )
              (i64.store offset=40
                (get_local $3)
                (i64.const 0)
              )
              ;; raybench.cpp:149
              (i32.store offset=48
                (get_local $3)
                (i32.const 0)
              )
              (i64.store
                (tee_local $4
                  (i32.add
                    (get_local $3)
                    (i32.const 56)
                  )
                )
                (i64.const 0)
              )
              (i64.store offset=8
                (get_local $4)
                (i64.const 0)
              )
              (i64.store offset=16
                (get_local $4)
                (i64.const 0)
              )
              ;; raybench.cpp:148
              (i64.store offset=24
                (get_local $4)
                (i64.const 0)
              )
              (i64.store
                (tee_local $4
                  (i32.add
                    (get_local $2)
                    (i32.const 8)
                  )
                )
                (i64.load
                  (get_local $3)
                )
              )
              (i64.store offset=8
                (get_local $4)
                (i64.load offset=8
                  (get_local $3)
                )
              )
              (i64.store offset=16
                (get_local $4)
                (i64.load offset=16
                  (get_local $3)
                )
              )
              (i64.store offset=24
                (get_local $4)
                (i64.load offset=24
                  (get_local $3)
                )
              )
              (i64.store offset=32
                (get_local $4)
                (i64.load offset=32
                  (get_local $3)
                )
              )
              (i64.store offset=40
                (get_local $4)
                (i64.load offset=40
                  (get_local $3)
                )
              )
              (i64.store offset=48
                (get_local $4)
                (i64.load offset=48
                  (get_local $3)
                )
              )
              (i64.store offset=56
                (get_local $4)
                (i64.load offset=56
                  (get_local $3)
                )
              )
              (i64.store offset=64
                (get_local $4)
                (i64.load offset=64
                  (get_local $3)
                )
              )
              (i64.store offset=72
                (get_local $4)
                (i64.load offset=72
                  (get_local $3)
                )
              )
              ;; raybench.cpp:277
              (i64.store offset=80
                (get_local $4)
                (i64.load offset=80
                  (get_local $3)
                )
              )
              ;; raybench.cpp:402
              (i32.store
                (get_local $2)
                (i32.const 1540)
              )
              ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:432
              (i32.store
                (tee_local $4
                  (i32.add
                    (get_local $2)
                    (i32.const 96)
                  )
                )
                (i32.const 0)
              )
              ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:433
              (i32.store offset=100
                (get_local $2)
                (i32.const 0)
              )
              ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:2252
              (i32.store offset=104
                (get_local $2)
                (i32.const 0)
              )
              ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1466
              (set_local $0
                (i32.load
                  (get_local $0)
                )
              )
              ;; raybench.cpp:403
              (i32.store
                (get_local $27)
                (get_local $0)
              )
              ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1482
              (set_local $0
                (i32.load
                  (get_local $32)
                )
              )
              ;; raybench.cpp:403
              (i32.store
                (get_local $28)
                (get_local $0)
              )
              ;; raybench.cpp:403
              (i32.store
                (get_local $26)
                (i32.load
                  (get_local $27)
                )
              )
              ;; raybench.cpp:403
              (i32.store
                (get_local $3)
                (i32.load
                  (get_local $28)
                )
              )
              ;; raybench.cpp:403
              (call $__ZNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEE6assignINS_11__wrap_iterIPS2_EEEENS_9enable_ifIXaasr21__is_forward_iteratorIT_EE5valuesr16is_constructibleIS2_NS_15iterator_traitsISB_E9referenceEEE5valueEvE4typeESB_SB_
                (get_local $4)
                (get_local $26)
                (get_local $3)
              )
              (set_local $6
                (get_local $2)
              )
              (set_local $2
                (i32.const 0)
              )
              (set_local $0
                (i32.const 0)
              )
              (set_local $7
                (i32.const 0)
              )
              (br $do-once15)
            )
            ;; raybench.cpp:907
            (set_local $0
              (i32.load
                (get_local $2)
              )
            )
            (set_local $5
              (get_local $6)
            )
            (set_local $2
              (get_local $23)
            )
            (br $__rjti$3)
          )
          (if
            (i32.eq
              (get_local $2)
              (get_local $5)
            )
            (block
              (set_local $11
                (f64.const 1.e+100)
              )
              (set_local $9
                (f64.const -1.e+100)
              )
              (set_local $20
                (f64.const -1.e+100)
              )
              (set_local $21
                (f64.const 1.e+100)
              )
              (set_local $22
                (f64.const -1.e+100)
              )
              (set_local $12
                (f64.const 1.e+100)
              )
            )
            (block
              ;; raybench.cpp:842
              (set_local $4
                (i32.add
                  (get_local $3)
                  (i32.const 8)
                )
              )
              ;; raybench.cpp:843
              (set_local $6
                (i32.add
                  (get_local $3)
                  (i32.const 16)
                )
              )
              ;; raybench.cpp:844
              (set_local $13
                (i32.add
                  (get_local $3)
                  (i32.const 24)
                )
              )
              ;; raybench.cpp:845
              (set_local $23
                (i32.add
                  (get_local $3)
                  (i32.const 32)
                )
              )
              ;; raybench.cpp:846
              (set_local $25
                (i32.add
                  (get_local $3)
                  (i32.const 40)
                )
              )
              (set_local $9
                (f64.const -1.e+100)
              )
              (set_local $20
                (f64.const -1.e+100)
              )
              (set_local $21
                (f64.const 1.e+100)
              )
              (set_local $22
                (f64.const -1.e+100)
              )
              (set_local $12
                (f64.const 1.e+100)
              )
              (set_local $11
                (f64.const 1.e+100)
              )
              (loop $while-in14
                ;; raybench.cpp:839
                (set_local $0
                  (i32.load
                    (get_local $2)
                  )
                )
                ;; raybench.cpp:840
                (set_local $24
                  (i32.load
                    (get_local $0)
                  )
                )
                ;; raybench.cpp:840
                (set_local $24
                  (i32.load offset=8
                    (get_local $24)
                  )
                )
                ;; raybench.cpp:840
                (call_indirect $FUNCSIG$vii
                  (get_local $3)
                  (get_local $0)
                  (i32.add
                    (i32.and
                      (get_local $24)
                      (i32.const 15)
                    )
                    (i32.const 36)
                  )
                )
                ;; raybench.cpp:841
                (set_local $8
                  (f64.load
                    (get_local $3)
                  )
                )
                ;; raybench.cpp:221
                (if
                  (i32.eqz
                    (f64.lt
                      (get_local $11)
                      (get_local $8)
                    )
                  )
                  (set_local $11
                    (get_local $8)
                  )
                )
                ;; raybench.cpp:842
                (set_local $8
                  (f64.load
                    (get_local $4)
                  )
                )
                ;; raybench.cpp:212
                (if
                  (i32.eqz
                    (f64.gt
                      (get_local $9)
                      (get_local $8)
                    )
                  )
                  (set_local $9
                    (get_local $8)
                  )
                )
                ;; raybench.cpp:843
                (set_local $8
                  (f64.load
                    (get_local $6)
                  )
                )
                ;; raybench.cpp:221
                (if
                  (i32.eqz
                    (f64.lt
                      (get_local $12)
                      (get_local $8)
                    )
                  )
                  (set_local $12
                    (get_local $8)
                  )
                )
                ;; raybench.cpp:844
                (set_local $8
                  (f64.load
                    (get_local $13)
                  )
                )
                ;; raybench.cpp:212
                (if
                  (i32.eqz
                    (f64.gt
                      (get_local $22)
                      (get_local $8)
                    )
                  )
                  (set_local $22
                    (get_local $8)
                  )
                )
                ;; raybench.cpp:845
                (set_local $8
                  (f64.load
                    (get_local $23)
                  )
                )
                ;; raybench.cpp:221
                (if
                  (i32.eqz
                    (f64.lt
                      (get_local $21)
                      (get_local $8)
                    )
                  )
                  (set_local $21
                    (get_local $8)
                  )
                )
                ;; raybench.cpp:846
                (set_local $8
                  (f64.load
                    (get_local $25)
                  )
                )
                ;; raybench.cpp:212
                (if
                  (i32.eqz
                    (f64.gt
                      (get_local $20)
                      (get_local $8)
                    )
                  )
                  (set_local $20
                    (get_local $8)
                  )
                )
                (br_if $while-in14
                  (i32.ne
                    (tee_local $2
                      (i32.add
                        (get_local $2)
                        (i32.const 4)
                      )
                    )
                    (get_local $5)
                  )
                )
              )
            )
          )
          ;; raybench.cpp:262
          (f64.store
            (get_local $10)
            (get_local $11)
          )
          ;; raybench.cpp:263
          (f64.store offset=8
            (get_local $10)
            (get_local $9)
          )
          ;; raybench.cpp:264
          (f64.store offset=16
            (get_local $10)
            (get_local $12)
          )
          ;; raybench.cpp:265
          (f64.store offset=24
            (get_local $10)
            (get_local $22)
          )
          ;; raybench.cpp:266
          (f64.store offset=32
            (get_local $10)
            (get_local $21)
          )
          ;; raybench.cpp:267
          (f64.store offset=40
            (get_local $10)
            (get_local $20)
          )
          (i64.store
            (get_local $3)
            (i64.load
              (get_local $10)
            )
          )
          (i64.store offset=8
            (get_local $3)
            (i64.load offset=8
              (get_local $10)
            )
          )
          (i64.store offset=16
            (get_local $3)
            (i64.load offset=16
              (get_local $10)
            )
          )
          (i64.store offset=24
            (get_local $3)
            (i64.load offset=24
              (get_local $10)
            )
          )
          (i64.store offset=32
            (get_local $3)
            (i64.load offset=32
              (get_local $10)
            )
          )
          ;; raybench.cpp:907
          (i64.store offset=40
            (get_local $3)
            (i64.load offset=40
              (get_local $10)
            )
          )
          ;; raybench.cpp:907
          (set_local $0
            (call $__Z9partitionRNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEEE6Boundsj
              (get_local $18)
              (get_local $3)
              (get_local $7)
            )
          )
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:640
          (set_local $4
            (i32.load
              (get_local $17)
            )
          )
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:640
          (set_local $2
            (i32.load
              (get_local $19)
            )
          )
          (set_local $5
            (get_local $4)
          )
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1499
        (set_local $6
          (get_local $2)
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/iterator:1366
        (if
          (i32.eq
            (i32.sub
              (get_local $5)
              (get_local $2)
            )
            (i32.const 4)
          )
          (block
            ;; raybench.cpp:908
            (set_local $2
              (i32.load
                (get_local $6)
              )
            )
            (set_local $6
              (i32.const 0)
            )
            (set_local $7
              (i32.const 1)
            )
            (br $do-once15)
          )
        )
        (if
          (i32.eq
            (get_local $6)
            (get_local $4)
          )
          (block
            (set_local $11
              (f64.const 1.e+100)
            )
            (set_local $9
              (f64.const -1.e+100)
            )
            (set_local $20
              (f64.const -1.e+100)
            )
            (set_local $21
              (f64.const 1.e+100)
            )
            (set_local $22
              (f64.const -1.e+100)
            )
            (set_local $12
              (f64.const 1.e+100)
            )
          )
          (block
            ;; raybench.cpp:842
            (set_local $5
              (i32.add
                (get_local $3)
                (i32.const 8)
              )
            )
            ;; raybench.cpp:843
            (set_local $13
              (i32.add
                (get_local $3)
                (i32.const 16)
              )
            )
            ;; raybench.cpp:844
            (set_local $23
              (i32.add
                (get_local $3)
                (i32.const 24)
              )
            )
            ;; raybench.cpp:845
            (set_local $25
              (i32.add
                (get_local $3)
                (i32.const 32)
              )
            )
            ;; raybench.cpp:846
            (set_local $24
              (i32.add
                (get_local $3)
                (i32.const 40)
              )
            )
            (set_local $9
              (f64.const -1.e+100)
            )
            (set_local $20
              (f64.const -1.e+100)
            )
            (set_local $21
              (f64.const 1.e+100)
            )
            (set_local $22
              (f64.const -1.e+100)
            )
            (set_local $12
              (f64.const 1.e+100)
            )
            (set_local $11
              (f64.const 1.e+100)
            )
            (set_local $2
              (get_local $6)
            )
            (loop $while-in18
              ;; raybench.cpp:839
              (set_local $6
                (i32.load
                  (get_local $2)
                )
              )
              ;; raybench.cpp:840
              (set_local $10
                (i32.load
                  (get_local $6)
                )
              )
              ;; raybench.cpp:840
              (set_local $10
                (i32.load offset=8
                  (get_local $10)
                )
              )
              ;; raybench.cpp:840
              (call_indirect $FUNCSIG$vii
                (get_local $3)
                (get_local $6)
                (i32.add
                  (i32.and
                    (get_local $10)
                    (i32.const 15)
                  )
                  (i32.const 36)
                )
              )
              ;; raybench.cpp:841
              (set_local $8
                (f64.load
                  (get_local $3)
                )
              )
              ;; raybench.cpp:221
              (if
                (i32.eqz
                  (f64.lt
                    (get_local $11)
                    (get_local $8)
                  )
                )
                (set_local $11
                  (get_local $8)
                )
              )
              ;; raybench.cpp:842
              (set_local $8
                (f64.load
                  (get_local $5)
                )
              )
              ;; raybench.cpp:212
              (if
                (i32.eqz
                  (f64.gt
                    (get_local $9)
                    (get_local $8)
                  )
                )
                (set_local $9
                  (get_local $8)
                )
              )
              ;; raybench.cpp:843
              (set_local $8
                (f64.load
                  (get_local $13)
                )
              )
              ;; raybench.cpp:221
              (if
                (i32.eqz
                  (f64.lt
                    (get_local $12)
                    (get_local $8)
                  )
                )
                (set_local $12
                  (get_local $8)
                )
              )
              ;; raybench.cpp:844
              (set_local $8
                (f64.load
                  (get_local $23)
                )
              )
              ;; raybench.cpp:212
              (if
                (i32.eqz
                  (f64.gt
                    (get_local $22)
                    (get_local $8)
                  )
                )
                (set_local $22
                  (get_local $8)
                )
              )
              ;; raybench.cpp:845
              (set_local $8
                (f64.load
                  (get_local $25)
                )
              )
              ;; raybench.cpp:221
              (if
                (i32.eqz
                  (f64.lt
                    (get_local $21)
                    (get_local $8)
                  )
                )
                (set_local $21
                  (get_local $8)
                )
              )
              ;; raybench.cpp:846
              (set_local $8
                (f64.load
                  (get_local $24)
                )
              )
              ;; raybench.cpp:212
              (if
                (i32.eqz
                  (f64.gt
                    (get_local $20)
                    (get_local $8)
                  )
                )
                (set_local $20
                  (get_local $8)
                )
              )
              (br_if $while-in18
                (i32.ne
                  (tee_local $2
                    (i32.add
                      (get_local $2)
                      (i32.const 4)
                    )
                  )
                  (get_local $4)
                )
              )
            )
          )
        )
        ;; raybench.cpp:262
        (f64.store
          (get_local $15)
          (get_local $11)
        )
        ;; raybench.cpp:263
        (f64.store offset=8
          (get_local $15)
          (get_local $9)
        )
        ;; raybench.cpp:264
        (f64.store offset=16
          (get_local $15)
          (get_local $12)
        )
        ;; raybench.cpp:265
        (f64.store offset=24
          (get_local $15)
          (get_local $22)
        )
        ;; raybench.cpp:266
        (f64.store offset=32
          (get_local $15)
          (get_local $21)
        )
        ;; raybench.cpp:267
        (f64.store offset=40
          (get_local $15)
          (get_local $20)
        )
        (i64.store
          (get_local $3)
          (i64.load
            (get_local $15)
          )
        )
        (i64.store offset=8
          (get_local $3)
          (i64.load offset=8
            (get_local $15)
          )
        )
        (i64.store offset=16
          (get_local $3)
          (i64.load offset=16
            (get_local $15)
          )
        )
        (i64.store offset=24
          (get_local $3)
          (i64.load offset=24
            (get_local $15)
          )
        )
        (i64.store offset=32
          (get_local $3)
          (i64.load offset=32
            (get_local $15)
          )
        )
        ;; raybench.cpp:908
        (i64.store offset=40
          (get_local $3)
          (i64.load offset=40
            (get_local $15)
          )
        )
        ;; raybench.cpp:908
        (set_local $2
          (call $__Z9partitionRNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEEE6Boundsj
            (get_local $19)
            (get_local $3)
            (get_local $7)
          )
        )
        (set_local $6
          (i32.const 0)
        )
        (set_local $7
          (i32.const 1)
        )
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:450
      (set_local $4
        (i32.load
          (get_local $19)
        )
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:450
      (set_local $5
        (get_local $4)
      )
      (if
        (get_local $4)
        (block
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:424
          (set_local $13
            (i32.load
              (get_local $17)
            )
          )
          (if
            (i32.ne
              (get_local $13)
              (get_local $4)
            )
            ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:425
            (i32.store
              (get_local $17)
              (i32.add
                (get_local $13)
                (i32.shl
                  (i32.xor
                    (i32.shr_u
                      (i32.sub
                        (i32.add
                          (get_local $13)
                          (i32.const -4)
                        )
                        (get_local $5)
                      )
                      (i32.const 2)
                    )
                    (i32.const -1)
                  )
                  (i32.const 2)
                )
              )
            )
          )
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/new:176
          (call $__ZdlPv
            (get_local $4)
          )
        )
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:450
      (set_local $4
        (i32.load
          (get_local $18)
        )
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:450
      (set_local $5
        (get_local $4)
      )
      (if
        (get_local $4)
        (block
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:424
          (set_local $13
            (i32.load
              (get_local $16)
            )
          )
          (if
            (i32.ne
              (get_local $13)
              (get_local $4)
            )
            ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:425
            (i32.store
              (get_local $16)
              (i32.add
                (get_local $13)
                (i32.shl
                  (i32.xor
                    (i32.shr_u
                      (i32.sub
                        (i32.add
                          (get_local $13)
                          (i32.const -4)
                        )
                        (get_local $5)
                      )
                      (i32.const 2)
                    )
                    (i32.const -1)
                  )
                  (i32.const 2)
                )
              )
            )
          )
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/new:176
          (call $__ZdlPv
            (get_local $4)
          )
        )
      )
      (if
        (i32.eqz
          (get_local $7)
        )
        (block
          (set_global $STACKTOP
            (get_local $14)
          )
          (return
            (get_local $6)
          )
        )
      )
    )
    ;; raybench.cpp:910
    (set_local $6
      (call $__Znwj
        (i32.const 152)
      )
    )
    (i64.store
      (get_local $3)
      (i64.const 0)
    )
    (i64.store offset=8
      (get_local $3)
      (i64.const 0)
    )
    (i64.store offset=16
      (get_local $3)
      (i64.const 0)
    )
    (i64.store offset=24
      (get_local $3)
      (i64.const 0)
    )
    (i64.store offset=32
      (get_local $3)
      (i64.const 0)
    )
    (i64.store offset=40
      (get_local $3)
      (i64.const 0)
    )
    ;; raybench.cpp:149
    (i32.store offset=48
      (get_local $3)
      (i32.const 0)
    )
    (i64.store
      (tee_local $4
        (i32.add
          (get_local $3)
          (i32.const 56)
        )
      )
      (i64.const 0)
    )
    (i64.store offset=8
      (get_local $4)
      (i64.const 0)
    )
    (i64.store offset=16
      (get_local $4)
      (i64.const 0)
    )
    ;; raybench.cpp:148
    (i64.store offset=24
      (get_local $4)
      (i64.const 0)
    )
    (i64.store
      (tee_local $4
        (i32.add
          (get_local $6)
          (i32.const 8)
        )
      )
      (i64.load
        (get_local $3)
      )
    )
    (i64.store offset=8
      (get_local $4)
      (i64.load offset=8
        (get_local $3)
      )
    )
    (i64.store offset=16
      (get_local $4)
      (i64.load offset=16
        (get_local $3)
      )
    )
    (i64.store offset=24
      (get_local $4)
      (i64.load offset=24
        (get_local $3)
      )
    )
    (i64.store offset=32
      (get_local $4)
      (i64.load offset=32
        (get_local $3)
      )
    )
    (i64.store offset=40
      (get_local $4)
      (i64.load offset=40
        (get_local $3)
      )
    )
    (i64.store offset=48
      (get_local $4)
      (i64.load offset=48
        (get_local $3)
      )
    )
    (i64.store offset=56
      (get_local $4)
      (i64.load offset=56
        (get_local $3)
      )
    )
    (i64.store offset=64
      (get_local $4)
      (i64.load offset=64
        (get_local $3)
      )
    )
    (i64.store offset=72
      (get_local $4)
      (i64.load offset=72
        (get_local $3)
      )
    )
    ;; raybench.cpp:277
    (i64.store offset=80
      (get_local $4)
      (i64.load offset=80
        (get_local $3)
      )
    )
    ;; raybench.cpp:299
    (i32.store
      (get_local $6)
      (i32.const 1568)
    )
    (i64.store
      (tee_local $4
        (i32.add
          (get_local $6)
          (i32.const 96)
        )
      )
      (i64.load
        (get_local $1)
      )
    )
    (i64.store offset=8
      (get_local $4)
      (i64.load offset=8
        (get_local $1)
      )
    )
    (i64.store offset=16
      (get_local $4)
      (i64.load offset=16
        (get_local $1)
      )
    )
    (i64.store offset=24
      (get_local $4)
      (i64.load offset=24
        (get_local $1)
      )
    )
    (i64.store offset=32
      (get_local $4)
      (i64.load offset=32
        (get_local $1)
      )
    )
    ;; raybench.cpp:296
    (i64.store offset=40
      (get_local $4)
      (i64.load offset=40
        (get_local $1)
      )
    )
    ;; raybench.cpp:297
    (i32.store offset=144
      (get_local $6)
      (get_local $0)
    )
    ;; raybench.cpp:298
    (i32.store offset=148
      (get_local $6)
      (get_local $2)
    )
    (set_global $STACKTOP
      (get_local $14)
    )
    (get_local $6)
  )
  (func $__ZN8Triangle9intersectERK4Vec3S2_ddPd (param $0 i32) (param $1 i32) (param $2 i32) (param $3 f64) (param $4 f64) (param $5 i32) (result i32)
    (local $6 f64)
    (local $7 f64)
    (local $8 f64)
    (local $9 f64)
    (local $10 f64)
    (local $11 f64)
    (local $12 f64)
    (local $13 f64)
    (local $14 f64)
    (local $15 f64)
    (local $16 f64)
    (local $17 f64)
    (local $18 f64)
    (local $19 f64)
    (local $20 f64)
    (local $21 f64)
    (local $22 f64)
    (local $23 f64)
    (local $24 f64)
    ;; raybench.cpp:518
    (set_local $7
      (f64.load offset=96
        (get_local $0)
      )
    )
    ;; raybench.cpp:518
    (set_local $11
      (f64.load offset=120
        (get_local $0)
      )
    )
    ;; raybench.cpp:519
    (set_local $8
      (f64.load offset=104
        (get_local $0)
      )
    )
    ;; raybench.cpp:519
    (set_local $13
      (f64.load offset=128
        (get_local $0)
      )
    )
    ;; raybench.cpp:520
    (set_local $9
      (f64.load offset=112
        (get_local $0)
      )
    )
    ;; raybench.cpp:520
    (set_local $14
      (f64.load offset=136
        (get_local $0)
      )
    )
    ;; raybench.cpp:521
    (set_local $10
      (f64.load offset=144
        (get_local $0)
      )
    )
    ;; raybench.cpp:522
    (set_local $6
      (f64.load offset=152
        (get_local $0)
      )
    )
    ;; raybench.cpp:523
    (set_local $15
      (f64.load offset=160
        (get_local $0)
      )
    )
    ;; raybench.cpp:524
    (set_local $17
      (f64.load
        (get_local $2)
      )
    )
    ;; raybench.cpp:525
    (set_local $18
      (f64.load offset=8
        (get_local $2)
      )
    )
    ;; raybench.cpp:526
    (set_local $19
      (f64.load offset=16
        (get_local $2)
      )
    )
    ;; raybench.cpp:527
    (set_local $16
      (f64.load
        (get_local $1)
      )
    )
    ;; raybench.cpp:528
    (set_local $22
      (f64.load offset=8
        (get_local $1)
      )
    )
    ;; raybench.cpp:529
    (set_local $23
      (f64.load offset=16
        (get_local $1)
      )
    )
    ;; raybench.cpp:530
    (set_local $20
      (f64.sub
        (f64.mul
          (tee_local $6
            (f64.sub
              (get_local $8)
              (get_local $6)
            )
          )
          (get_local $19)
        )
        (f64.mul
          (tee_local $15
            (f64.sub
              (get_local $9)
              (get_local $15)
            )
          )
          (get_local $18)
        )
      )
    )
    ;; raybench.cpp:530
    (set_local $21
      (f64.sub
        (f64.mul
          (get_local $17)
          (get_local $15)
        )
        (f64.mul
          (tee_local $10
            (f64.sub
              (get_local $7)
              (get_local $10)
            )
          )
          (get_local $19)
        )
      )
    )
    ;; raybench.cpp:530
    (set_local $11
      (f64.add
        (f64.mul
          (tee_local $14
            (f64.sub
              (get_local $9)
              (get_local $14)
            )
          )
          (tee_local $24
            (f64.sub
              (f64.mul
                (get_local $10)
                (get_local $18)
              )
              (f64.mul
                (get_local $6)
                (get_local $17)
              )
            )
          )
        )
        (f64.add
          (f64.mul
            (tee_local $12
              (f64.sub
                (get_local $7)
                (get_local $11)
              )
            )
            (get_local $20)
          )
          (f64.mul
            (tee_local $13
              (f64.sub
                (get_local $8)
                (get_local $13)
              )
            )
            (get_local $21)
          )
        )
      )
    )
    ;; raybench.cpp:531
    (set_local $16
      (f64.sub
        (f64.mul
          (get_local $12)
          (tee_local $8
            (f64.sub
              (get_local $8)
              (get_local $22)
            )
          )
        )
        (f64.mul
          (get_local $13)
          (tee_local $7
            (f64.sub
              (get_local $7)
              (get_local $16)
            )
          )
        )
      )
    )
    ;; raybench.cpp:531
    (set_local $12
      (f64.sub
        (f64.mul
          (get_local $14)
          (get_local $7)
        )
        (f64.mul
          (get_local $12)
          (tee_local $9
            (f64.sub
              (get_local $9)
              (get_local $23)
            )
          )
        )
      )
    )
    ;; raybench.cpp:534
    (if
      (i32.or
        (f64.lt
          (tee_local $6
            (f64.neg
              (f64.div
                (f64.add
                  (f64.mul
                    (get_local $10)
                    (tee_local $10
                      (f64.sub
                        (f64.mul
                          (get_local $13)
                          (get_local $9)
                        )
                        (f64.mul
                          (get_local $14)
                          (get_local $8)
                        )
                      )
                    )
                  )
                  (f64.add
                    (f64.mul
                      (get_local $15)
                      (get_local $16)
                    )
                    (f64.mul
                      (get_local $6)
                      (get_local $12)
                    )
                  )
                )
                (get_local $11)
              )
            )
          )
          (get_local $3)
        )
        (f64.gt
          (get_local $6)
          (get_local $4)
        )
      )
      (return
        (i32.const 0)
      )
    )
    ;; raybench.cpp:537
    (if
      (i32.or
        (f64.lt
          (tee_local $3
            (f64.div
              (f64.add
                (f64.mul
                  (get_local $17)
                  (get_local $10)
                )
                (f64.add
                  (f64.mul
                    (get_local $19)
                    (get_local $16)
                  )
                  (f64.mul
                    (get_local $18)
                    (get_local $12)
                  )
                )
              )
              (get_local $11)
            )
          )
          (f64.const 0)
        )
        (f64.gt
          (get_local $3)
          (f64.const 1)
        )
      )
      (return
        (i32.const 0)
      )
    )
    (if
      (i32.or
        (f64.lt
          (tee_local $4
            (f64.div
              (f64.add
                (f64.add
                  (f64.mul
                    (get_local $7)
                    (get_local $20)
                  )
                  (f64.mul
                    (get_local $21)
                    (get_local $8)
                  )
                )
                (f64.mul
                  (get_local $24)
                  (get_local $9)
                )
              )
              (get_local $11)
            )
          )
          (f64.const 0)
        )
        (f64.gt
          (get_local $4)
          (f64.sub
            (f64.const 1)
            (get_local $3)
          )
        )
      )
      (return
        (i32.const 0)
      )
    )
    ;; raybench.cpp:540
    (f64.store
      (get_local $5)
      (get_local $6)
    )
    (get_local $0)
  )
  (func $__ZN8Triangle6normalERK4Vec3 (param $0 i32) (param $1 i32) (param $2 i32)
    (local $3 i32)
    (i64.store
      (get_local $0)
      (i64.load
        (tee_local $3
          (i32.add
            (get_local $1)
            (i32.const 168)
          )
        )
      )
    )
    (i64.store offset=8
      (get_local $0)
      (i64.load offset=8
        (get_local $3)
      )
    )
    ;; raybench.cpp:545
    (i64.store offset=16
      (get_local $0)
      (i64.load offset=16
        (get_local $3)
      )
    )
  )
  (func $__ZN8Triangle6boundsEv (param $0 i32) (param $1 i32)
    (local $2 f64)
    (local $3 f64)
    (local $4 f64)
    (local $5 f64)
    (local $6 f64)
    (local $7 f64)
    (local $8 f64)
    (local $9 f64)
    (local $10 f64)
    (local $11 f64)
    ;; raybench.cpp:549
    (set_local $2
      (f64.load offset=96
        (get_local $1)
      )
    )
    ;; raybench.cpp:549
    (set_local $5
      (f64.load offset=120
        (get_local $1)
      )
    )
    ;; raybench.cpp:549
    (set_local $6
      (f64.load offset=144
        (get_local $1)
      )
    )
    ;; raybench.cpp:550
    (set_local $3
      (f64.load offset=104
        (get_local $1)
      )
    )
    ;; raybench.cpp:550
    (set_local $7
      (f64.load offset=128
        (get_local $1)
      )
    )
    ;; raybench.cpp:550
    (set_local $8
      (f64.load offset=152
        (get_local $1)
      )
    )
    ;; raybench.cpp:551
    (set_local $4
      (f64.load offset=112
        (get_local $1)
      )
    )
    ;; raybench.cpp:551
    (set_local $9
      (f64.load offset=136
        (get_local $1)
      )
    )
    ;; raybench.cpp:551
    (set_local $10
      (f64.load offset=160
        (get_local $1)
      )
    )
    ;; raybench.cpp:262
    (f64.store
      (get_local $0)
      (if f64
        (f64.gt
          (tee_local $11
            (if f64
              (f64.lt
                (get_local $2)
                (get_local $5)
              )
              (get_local $2)
              (get_local $5)
            )
          )
          (get_local $6)
        )
        (get_local $6)
        (get_local $11)
      )
    )
    ;; raybench.cpp:263
    (f64.store offset=8
      (get_local $0)
      (if f64
        (f64.lt
          (if f64
            (f64.gt
              (get_local $2)
              (get_local $5)
            )
            (get_local $2)
            (tee_local $2
              (get_local $5)
            )
          )
          (get_local $6)
        )
        (get_local $6)
        (get_local $2)
      )
    )
    ;; raybench.cpp:264
    (f64.store offset=16
      (get_local $0)
      (if f64
        (f64.gt
          (tee_local $2
            (if f64
              (f64.lt
                (get_local $3)
                (get_local $7)
              )
              (get_local $3)
              (get_local $7)
            )
          )
          (get_local $8)
        )
        (get_local $8)
        (get_local $2)
      )
    )
    ;; raybench.cpp:265
    (f64.store offset=24
      (get_local $0)
      (if f64
        (f64.lt
          (if f64
            (f64.gt
              (get_local $3)
              (get_local $7)
            )
            (get_local $3)
            (tee_local $3
              (get_local $7)
            )
          )
          (get_local $8)
        )
        (get_local $8)
        (get_local $3)
      )
    )
    ;; raybench.cpp:266
    (f64.store offset=32
      (get_local $0)
      (if f64
        (f64.gt
          (tee_local $2
            (if f64
              (f64.lt
                (get_local $4)
                (get_local $9)
              )
              (get_local $4)
              (get_local $9)
            )
          )
          (get_local $10)
        )
        (get_local $10)
        (get_local $2)
      )
    )
    ;; raybench.cpp:267
    (f64.store offset=40
      (get_local $0)
      (if f64
        (f64.lt
          (if f64
            (f64.gt
              (get_local $4)
              (get_local $9)
            )
            (get_local $4)
            (tee_local $4
              (get_local $9)
            )
          )
          (get_local $10)
        )
        (get_local $10)
        (get_local $4)
      )
    )
  )
  (func $__ZN8Triangle6centerEv (param $0 i32) (param $1 i32)
    (local $2 f64)
    (local $3 f64)
    (local $4 f64)
    (local $5 f64)
    (local $6 f64)
    (local $7 f64)
    (local $8 f64)
    (local $9 f64)
    (local $10 f64)
    ;; raybench.cpp:555
    (set_local $2
      (f64.load offset=96
        (get_local $1)
      )
    )
    ;; raybench.cpp:555
    (set_local $3
      (f64.load offset=120
        (get_local $1)
      )
    )
    ;; raybench.cpp:555
    (set_local $4
      (f64.load offset=144
        (get_local $1)
      )
    )
    ;; raybench.cpp:556
    (set_local $5
      (f64.load offset=104
        (get_local $1)
      )
    )
    ;; raybench.cpp:556
    (set_local $6
      (f64.load offset=128
        (get_local $1)
      )
    )
    ;; raybench.cpp:556
    (set_local $7
      (f64.load offset=152
        (get_local $1)
      )
    )
    ;; raybench.cpp:557
    (set_local $8
      (f64.load offset=112
        (get_local $1)
      )
    )
    ;; raybench.cpp:557
    (set_local $9
      (f64.load offset=136
        (get_local $1)
      )
    )
    ;; raybench.cpp:557
    (set_local $10
      (f64.load offset=160
        (get_local $1)
      )
    )
    ;; raybench.cpp:148
    (f64.store
      (get_local $0)
      (f64.div
        (f64.add
          (f64.add
            (get_local $2)
            (get_local $3)
          )
          (get_local $4)
        )
        (f64.const 3)
      )
    )
    ;; raybench.cpp:149
    (f64.store offset=8
      (get_local $0)
      (f64.div
        (f64.add
          (f64.add
            (get_local $5)
            (get_local $6)
          )
          (get_local $7)
        )
        (f64.const 3)
      )
    )
    ;; raybench.cpp:150
    (f64.store offset=16
      (get_local $0)
      (f64.div
        (f64.add
          (f64.add
            (get_local $8)
            (get_local $9)
          )
          (get_local $10)
        )
        (f64.const 3)
      )
    )
  )
  (func $__ZN8Triangle5debugEPFvPKcEj (param $0 i32) (param $1 i32) (param $2 i32)
    (local $3 f64)
    (local $4 f64)
    (local $5 f64)
    (local $6 f64)
    (local $7 f64)
    (local $8 f64)
    (local $9 f64)
    (local $10 f64)
    (local $11 f64)
    (local $12 i32)
    (set_local $2
      (get_global $STACKTOP)
    )
    ;; raybench.cpp:562
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 1104)
      )
    )
    ;; raybench.cpp:562
    (set_local $3
      (f64.load offset=96
        (get_local $0)
      )
    )
    ;; raybench.cpp:562
    (set_local $4
      (f64.load offset=104
        (get_local $0)
      )
    )
    ;; raybench.cpp:562
    (set_local $5
      (f64.load offset=112
        (get_local $0)
      )
    )
    ;; raybench.cpp:562
    (set_local $6
      (f64.load offset=120
        (get_local $0)
      )
    )
    ;; raybench.cpp:562
    (set_local $7
      (f64.load offset=128
        (get_local $0)
      )
    )
    ;; raybench.cpp:562
    (set_local $8
      (f64.load offset=136
        (get_local $0)
      )
    )
    ;; raybench.cpp:562
    (set_local $9
      (f64.load offset=144
        (get_local $0)
      )
    )
    ;; raybench.cpp:562
    (set_local $10
      (f64.load offset=152
        (get_local $0)
      )
    )
    ;; raybench.cpp:562
    (set_local $11
      (f64.load offset=160
        (get_local $0)
      )
    )
    ;; raybench.cpp:562
    (f64.store
      (tee_local $0
        (get_local $2)
      )
      (get_local $3)
    )
    ;; raybench.cpp:562
    (f64.store offset=8
      (get_local $0)
      (get_local $4)
    )
    ;; raybench.cpp:562
    (f64.store offset=16
      (get_local $0)
      (get_local $5)
    )
    ;; raybench.cpp:562
    (f64.store offset=24
      (get_local $0)
      (get_local $6)
    )
    ;; raybench.cpp:562
    (f64.store offset=32
      (get_local $0)
      (get_local $7)
    )
    ;; raybench.cpp:562
    (f64.store offset=40
      (get_local $0)
      (get_local $8)
    )
    ;; raybench.cpp:562
    (f64.store offset=48
      (get_local $0)
      (get_local $9)
    )
    ;; raybench.cpp:562
    (f64.store offset=56
      (get_local $0)
      (get_local $10)
    )
    ;; raybench.cpp:562
    (f64.store offset=64
      (get_local $0)
      (get_local $11)
    )
    ;; raybench.cpp:562
    (drop
      (call $_sprintf
        (tee_local $12
          (i32.add
            (get_local $2)
            (i32.const 72)
          )
        )
        (i32.const 2442)
        (get_local $0)
      )
    )
    ;; raybench.cpp:563
    (call_indirect $FUNCSIG$vi
      (get_local $12)
      (i32.add
        (i32.and
          (get_local $1)
          (i32.const 15)
        )
        (i32.const 12)
      )
    )
    (set_global $STACKTOP
      (get_local $2)
    )
  )
  (func $__ZNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEE6assignINS_11__wrap_iterIPS2_EEEENS_9enable_ifIXaasr21__is_forward_iteratorIT_EE5valuesr16is_constructibleIS2_NS_15iterator_traitsISB_E9referenceEEE5valueEvE4typeESB_SB_ (param $0 i32) (param $1 i32) (param $2 i32)
    (local $3 i32)
    (local $4 i32)
    (local $5 i32)
    (local $6 i32)
    (local $7 i32)
    (local $8 i32)
    (local $9 i32)
    (local $10 i32)
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1391
    (set_local $5
      (i32.load
        (get_local $1)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1391
    (set_local $7
      (i32.load
        (get_local $2)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:372
    (set_local $8
      (i32.load
        (tee_local $9
          (i32.add
            (get_local $0)
            (i32.const 8)
          )
        )
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:372
    (set_local $1
      (i32.load
        (get_local $0)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1402
    (set_local $3
      (get_local $5)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1402
    (set_local $10
      (get_local $1)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:940
    (if
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:640
      (i32.le_u
        (tee_local $6
          (i32.shr_s
            (i32.sub
              (get_local $7)
              (get_local $5)
            )
            (i32.const 2)
          )
        )
        (i32.shr_s
          (i32.sub
            (get_local $8)
            (get_local $1)
          )
          (i32.const 2)
        )
      )
      (block
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:640
        (set_local $0
          (i32.load
            (tee_local $4
              (i32.add
                (get_local $0)
                (i32.const 4)
              )
            )
          )
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1396
        (set_local $6
          (i32.gt_u
            (get_local $6)
            (tee_local $0
              (i32.shr_s
                (i32.sub
                  (get_local $0)
                  (get_local $1)
                )
                (i32.const 2)
              )
            )
          )
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/iterator:1222
        (set_local $0
          (i32.add
            (get_local $3)
            (i32.shl
              (get_local $0)
              (i32.const 2)
            )
          )
        )
        (if
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/algorithm:1756
          (tee_local $7
            (i32.shr_s
              (tee_local $3
                (i32.sub
                  (if i32
                    (get_local $6)
                    (get_local $0)
                    (tee_local $0
                      (get_local $7)
                    )
                  )
                  (get_local $5)
                )
              )
              (i32.const 2)
            )
          )
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/algorithm:1756
          (drop
            (call $_memmove
              (get_local $1)
              (get_local $5)
              (get_local $3)
            )
          )
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/algorithm:1757
        (set_local $1
          (i32.add
            (get_local $10)
            (i32.shl
              (get_local $7)
              (i32.const 2)
            )
          )
        )
        (if
          (i32.eqz
            (get_local $6)
          )
          (block
            ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:424
            (set_local $0
              (i32.load
                (get_local $4)
              )
            )
            ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:813
            (if
              (i32.eq
                (get_local $0)
                (get_local $1)
              )
              (return)
            )
            ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:425
            (i32.store
              (get_local $4)
              (i32.add
                (get_local $0)
                (i32.shl
                  (i32.xor
                    (i32.shr_u
                      (i32.sub
                        (i32.add
                          (get_local $0)
                          (i32.const -4)
                        )
                        (get_local $1)
                      )
                      (i32.const 2)
                    )
                    (i32.const -1)
                  )
                  (i32.const 2)
                )
              )
            )
            (return)
          )
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1404
        (set_local $2
          (i32.load
            (get_local $2)
          )
        )
        (if
          (i32.eq
            (get_local $0)
            (get_local $2)
          )
          (return)
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1586
        (set_local $1
          (i32.load
            (get_local $4)
          )
        )
        (loop $while-in
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
          (set_local $3
            (i32.load
              (get_local $0)
            )
          )
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
          (i32.store
            (get_local $1)
            (get_local $3)
          )
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1585
          (set_local $1
            (i32.load
              (get_local $4)
            )
          )
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1585
          (i32.store
            (get_local $4)
            (tee_local $1
              (i32.add
                (get_local $1)
                (i32.const 4)
              )
            )
          )
          (br_if $while-in
            (i32.ne
              (tee_local $0
                (i32.add
                  (get_local $0)
                  (i32.const 4)
                )
              )
              (get_local $2)
            )
          )
        )
        (return)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:943
    (set_local $2
      (get_local $1)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:962
    (if
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:424
      (get_local $1)
      (block
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:424
        (set_local $5
          (i32.load
            (tee_local $8
              (i32.add
                (get_local $0)
                (i32.const 4)
              )
            )
          )
        )
        (if
          (i32.ne
            (get_local $5)
            (get_local $10)
          )
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:425
          (i32.store
            (get_local $8)
            (i32.add
              (get_local $5)
              (i32.shl
                (i32.xor
                  (i32.shr_u
                    (i32.sub
                      (i32.add
                        (get_local $5)
                        (i32.const -4)
                      )
                      (get_local $1)
                    )
                    (i32.const 2)
                  )
                  (i32.const -1)
                )
                (i32.const 2)
              )
            )
          )
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/new:176
        (call $__ZdlPv
          (get_local $2)
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:944
        (i32.store
          (get_local $9)
          (i32.const 0)
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:944
        (i32.store
          (get_local $8)
          (i32.const 0)
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:944
        (i32.store
          (get_local $0)
          (i32.const 0)
        )
        (set_local $8
          (i32.const 0)
        )
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:372
    (if
      (i32.gt_u
        (get_local $6)
        (i32.const 1073741823)
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:963
      (call $__ZNKSt3__220__vector_base_commonILb1EE20__throw_length_errorEv
        (get_local $0)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1736
    (if
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:967
      (i32.lt_u
        (i32.shr_s
          (tee_local $1
            (i32.sub
              (get_local $8)
              (i32.const 0)
            )
          )
          (i32.const 2)
        )
        (i32.const 536870911)
      )
      (if
        (i32.gt_u
          (if i32
            (i32.lt_u
              (tee_local $1
                (i32.shr_s
                  (get_local $1)
                  (i32.const 1)
                )
              )
              (get_local $6)
            )
            (tee_local $1
              (get_local $6)
            )
            (get_local $1)
          )
          (i32.const 1073741823)
        )
        ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:930
        (call $__ZNKSt3__220__vector_base_commonILb1EE20__throw_length_errorEv
          (get_local $0)
        )
        (set_local $4
          (get_local $1)
        )
      )
      (set_local $4
        (i32.const 1073741823)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/new:168
    (set_local $1
      (call $__Znwj
        (i32.shl
          (get_local $4)
          (i32.const 2)
        )
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:931
    (i32.store
      (tee_local $2
        (i32.add
          (get_local $0)
          (i32.const 4)
        )
      )
      (get_local $1)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:931
    (i32.store
      (get_local $0)
      (get_local $1)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:932
    (i32.store
      (get_local $9)
      (i32.add
        (get_local $1)
        (i32.shl
          (get_local $4)
          (i32.const 2)
        )
      )
    )
    (if
      (i32.eq
        (get_local $3)
        (get_local $7)
      )
      (return)
    )
    (set_local $0
      (get_local $3)
    )
    (loop $while-in1
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
      (set_local $3
        (i32.load
          (get_local $0)
        )
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
      (i32.store
        (get_local $1)
        (get_local $3)
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1585
      (set_local $1
        (i32.load
          (get_local $2)
        )
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1585
      (i32.store
        (get_local $2)
        (tee_local $1
          (i32.add
            (get_local $1)
            (i32.const 4)
          )
        )
      )
      (br_if $while-in1
        (i32.ne
          (tee_local $0
            (i32.add
              (get_local $0)
              (i32.const 4)
            )
          )
          (get_local $7)
        )
      )
    )
  )
  (func $__ZN6Jumble9intersectERK4Vec3S2_ddPd (param $0 i32) (param $1 i32) (param $2 i32) (param $3 f64) (param $4 f64) (param $5 i32) (result i32)
    (local $6 i32)
    (local $7 i32)
    (local $8 i32)
    (local $9 i32)
    (local $10 f64)
    (local $11 i32)
    (local $12 i32)
    (local $13 f64)
    (set_local $8
      (get_global $STACKTOP)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1466
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 16)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1466
    (set_local $7
      (i32.load offset=96
        (get_local $0)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:1482
    (set_local $12
      (i32.load offset=100
        (get_local $0)
      )
    )
    (if
      (i32.eq
        (get_local $7)
        (get_local $12)
      )
      (block
        ;; raybench.cpp:420
        (f64.store
          (get_local $5)
          (f64.const 1.e+100)
        )
        (set_global $STACKTOP
          (get_local $8)
        )
        (return
          (i32.const 0)
        )
      )
    )
    (set_local $9
      (get_local $8)
    )
    (set_local $10
      (f64.const 1.e+100)
    )
    (set_local $0
      (i32.const 0)
    )
    (loop $while-in
      ;; raybench.cpp:410
      (set_local $6
        (i32.load
          (get_local $7)
        )
      )
      ;; raybench.cpp:411
      (f64.store
        (get_local $9)
        (f64.const 0)
      )
      ;; raybench.cpp:412
      (set_local $11
        (i32.load
          (get_local $6)
        )
      )
      ;; raybench.cpp:412
      (set_local $11
        (i32.load
          (get_local $11)
        )
      )
      ;; raybench.cpp:412
      (set_local $6
        (call_indirect $FUNCSIG$iiiiddi
          (get_local $6)
          (get_local $1)
          (get_local $2)
          (get_local $3)
          (get_local $4)
          (get_local $9)
          (i32.add
            (i32.and
              (get_local $11)
              (i32.const 7)
            )
            (i32.const 28)
          )
        )
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/iterator:1198
      (if
        (get_local $6)
        (block
          ;; raybench.cpp:414
          (set_local $13
            (f64.load
              (get_local $9)
            )
          )
          (if
            (f64.lt
              (get_local $13)
              (get_local $10)
            )
            (block
              (set_local $0
                (get_local $6)
              )
              (set_local $10
                (get_local $13)
              )
            )
          )
        )
      )
      (br_if $while-in
        (i32.ne
          (tee_local $7
            (i32.add
              (get_local $7)
              (i32.const 4)
            )
          )
          (get_local $12)
        )
      )
    )
    ;; raybench.cpp:420
    (f64.store
      (get_local $5)
      (get_local $10)
    )
    (set_global $STACKTOP
      (get_local $8)
    )
    (get_local $0)
  )
  (func $__ZN6Jumble6normalERK4Vec3 (param $0 i32) (param $1 i32) (param $2 i32)
    ;; raybench.cpp:425
    (call $__Z5CRASHPKc
      (i32.const 2487)
    )
  )
  (func $__ZN6Jumble6boundsEv (param $0 i32) (param $1 i32)
    ;; raybench.cpp:430
    (call $__Z5CRASHPKc
      (i32.const 2521)
    )
  )
  (func $__ZN6Jumble6centerEv (param $0 i32) (param $1 i32)
    ;; raybench.cpp:435
    (call $__Z5CRASHPKc
      (i32.const 2555)
    )
  )
  (func $__ZN6Jumble5debugEPFvPKcEj (param $0 i32) (param $1 i32) (param $2 i32)
    (nop)
  )
  (func $__ZN6Volume9intersectERK4Vec3S2_ddPd (param $0 i32) (param $1 i32) (param $2 i32) (param $3 f64) (param $4 f64) (param $5 i32) (result i32)
    (local $6 f64)
    (local $7 f64)
    (local $8 f64)
    (local $9 f64)
    (local $10 i32)
    (local $11 f64)
    (local $12 i32)
    (local $13 i32)
    (local $14 f64)
    (local $15 f64)
    (local $16 i32)
    (local $17 i32)
    (set_local $13
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 16)
      )
    )
    ;; raybench.cpp:305
    (set_local $6
      (f64.load
        (get_local $2)
      )
    )
    ;; raybench.cpp:311
    (set_local $8
      (f64.load offset=104
        (get_local $0)
      )
    )
    ;; raybench.cpp:311
    (set_local $7
      (f64.load
        (get_local $1)
      )
    )
    ;; raybench.cpp:312
    (set_local $9
      (f64.load offset=96
        (get_local $0)
      )
    )
    ;; raybench.cpp:306
    (set_local $6
      (if f64
        (tee_local $10
          (i32.eqz
            (f64.ge
              (tee_local $11
                (f64.div
                  (f64.const 1)
                  (get_local $6)
                )
              )
              (f64.const 0)
            )
          )
        )
        (get_local $9)
        (get_local $8)
      )
    )
    ;; raybench.cpp:307
    (set_local $8
      (f64.mul
        (get_local $11)
        (f64.sub
          (if f64
            (get_local $10)
            (get_local $8)
            (get_local $9)
          )
          (get_local $7)
        )
      )
    )
    ;; raybench.cpp:308
    (set_local $7
      (f64.mul
        (get_local $11)
        (f64.sub
          (get_local $6)
          (get_local $7)
        )
      )
    )
    ;; raybench.cpp:316
    (set_local $6
      (f64.load offset=8
        (get_local $2)
      )
    )
    ;; raybench.cpp:322
    (set_local $11
      (f64.load offset=120
        (get_local $0)
      )
    )
    ;; raybench.cpp:322
    (set_local $14
      (f64.load offset=8
        (get_local $1)
      )
    )
    ;; raybench.cpp:323
    (set_local $9
      (f64.load offset=112
        (get_local $0)
      )
    )
    ;; raybench.cpp:317
    (set_local $6
      (if f64
        (tee_local $10
          (i32.eqz
            (f64.ge
              (tee_local $15
                (f64.div
                  (f64.const 1)
                  (get_local $6)
                )
              )
              (f64.const 0)
            )
          )
        )
        (get_local $9)
        (get_local $11)
      )
    )
    ;; raybench.cpp:328
    (if
      (i32.or
        (f64.gt
          (get_local $8)
          (tee_local $6
            (f64.mul
              (get_local $15)
              (f64.sub
                (get_local $6)
                (get_local $14)
              )
            )
          )
        )
        (f64.gt
          (tee_local $9
            (f64.mul
              (get_local $15)
              (f64.sub
                (if f64
                  (get_local $10)
                  (get_local $11)
                  (get_local $9)
                )
                (get_local $14)
              )
            )
          )
          (get_local $7)
        )
      )
      (block
        (set_global $STACKTOP
          (get_local $13)
        )
        (return
          (i32.const 0)
        )
      )
    )
    ;; raybench.cpp:328
    (if
      (i32.eqz
        (f64.gt
          (get_local $9)
          (get_local $8)
        )
      )
      (set_local $9
        (get_local $8)
      )
    )
    ;; raybench.cpp:330
    (if
      (i32.eqz
        (f64.lt
          (get_local $6)
          (get_local $7)
        )
      )
      (set_local $6
        (get_local $7)
      )
    )
    ;; raybench.cpp:333
    (set_local $7
      (f64.load offset=16
        (get_local $2)
      )
    )
    ;; raybench.cpp:339
    (set_local $8
      (f64.load offset=136
        (get_local $0)
      )
    )
    ;; raybench.cpp:339
    (set_local $11
      (f64.load offset=16
        (get_local $1)
      )
    )
    ;; raybench.cpp:340
    (set_local $14
      (f64.load offset=128
        (get_local $0)
      )
    )
    ;; raybench.cpp:334
    (set_local $7
      (if f64
        (tee_local $10
          (i32.eqz
            (f64.ge
              (tee_local $15
                (f64.div
                  (f64.const 1)
                  (get_local $7)
                )
              )
              (f64.const 0)
            )
          )
        )
        (get_local $8)
        (get_local $14)
      )
    )
    ;; raybench.cpp:345
    (if
      (i32.or
        (f64.gt
          (tee_local $7
            (f64.mul
              (get_local $15)
              (f64.sub
                (get_local $7)
                (get_local $11)
              )
            )
          )
          (get_local $6)
        )
        (f64.gt
          (get_local $9)
          (tee_local $8
            (f64.mul
              (get_local $15)
              (f64.sub
                (if f64
                  (get_local $10)
                  (get_local $14)
                  (get_local $8)
                )
                (get_local $11)
              )
            )
          )
        )
      )
      (block
        (set_global $STACKTOP
          (get_local $13)
        )
        (return
          (i32.const 0)
        )
      )
    )
    (if
      (i32.eqz
        (i32.and
          (f64.lt
            (if f64
              (f64.gt
                (get_local $7)
                (get_local $9)
              )
              (get_local $7)
              (get_local $9)
            )
            (get_local $4)
          )
          (f64.gt
            (if f64
              (f64.lt
                (get_local $8)
                (get_local $6)
              )
              (get_local $8)
              (get_local $6)
            )
            (get_local $3)
          )
        )
      )
      (block
        (set_global $STACKTOP
          (get_local $13)
        )
        (return
          (i32.const 0)
        )
      )
    )
    (set_local $10
      (get_local $13)
    )
    ;; raybench.cpp:354
    (f64.store
      (tee_local $17
        (i32.add
          (get_local $13)
          (i32.const 8)
        )
      )
      (f64.const 0)
    )
    ;; raybench.cpp:355
    (set_local $16
      (i32.load offset=144
        (get_local $0)
      )
    )
    ;; raybench.cpp:355
    (set_local $12
      (i32.load
        (get_local $16)
      )
    )
    ;; raybench.cpp:355
    (set_local $12
      (i32.load
        (get_local $12)
      )
    )
    ;; raybench.cpp:355
    (set_local $16
      (call_indirect $FUNCSIG$iiiiddi
        (get_local $16)
        (get_local $1)
        (get_local $2)
        (get_local $3)
        (get_local $4)
        (get_local $17)
        (i32.add
          (i32.and
            (get_local $12)
            (i32.const 7)
          )
          (i32.const 28)
        )
      )
    )
    ;; raybench.cpp:358
    (set_local $0
      (i32.load offset=148
        (get_local $0)
      )
    )
    (block $__rjto$0
      (if
        (get_local $0)
        (block
          ;; raybench.cpp:357
          (f64.store
            (get_local $10)
            (f64.const 0)
          )
          ;; raybench.cpp:358
          (set_local $12
            (i32.load
              (get_local $0)
            )
          )
          ;; raybench.cpp:358
          (set_local $12
            (i32.load
              (get_local $12)
            )
          )
          ;; raybench.cpp:358
          (set_local $0
            (call_indirect $FUNCSIG$iiiiddi
              (get_local $0)
              (get_local $1)
              (get_local $2)
              (get_local $3)
              (get_local $4)
              (get_local $10)
              (i32.add
                (i32.and
                  (get_local $12)
                  (i32.const 7)
                )
                (i32.const 28)
              )
            )
          )
          (if
            ;; raybench.cpp:359
            (get_local $0)
            (block
              ;; raybench.cpp:360
              (set_local $3
                (f64.load
                  (get_local $10)
                )
              )
              ;; raybench.cpp:359
              (set_local $4
                (f64.load
                  (get_local $17)
                )
              )
              (if
                (i32.or
                  (i32.eqz
                    (get_local $16)
                  )
                  (f64.lt
                    (get_local $3)
                    (get_local $4)
                  )
                )
                (block
                  ;; raybench.cpp:360
                  (f64.store
                    (get_local $5)
                    (get_local $3)
                  )
                  (br $__rjto$0)
                )
              )
            )
          )
        )
      )
      ;; raybench.cpp:364
      (set_local $3
        (f64.load
          (get_local $17)
        )
      )
      ;; raybench.cpp:364
      (f64.store
        (get_local $5)
        (get_local $3)
      )
      (set_local $0
        (get_local $16)
      )
    )
    (set_global $STACKTOP
      (get_local $13)
    )
    (get_local $0)
  )
  (func $__ZN6Volume6normalERK4Vec3 (param $0 i32) (param $1 i32) (param $2 i32)
    ;; raybench.cpp:373
    (call $__Z5CRASHPKc
      (i32.const 2597)
    )
  )
  (func $__ZN6Volume6boundsEv (param $0 i32) (param $1 i32)
    (local $2 i32)
    (i64.store
      (get_local $0)
      (i64.load
        (tee_local $2
          (i32.add
            (get_local $1)
            (i32.const 96)
          )
        )
      )
    )
    (i64.store offset=8
      (get_local $0)
      (i64.load offset=8
        (get_local $2)
      )
    )
    (i64.store offset=16
      (get_local $0)
      (i64.load offset=16
        (get_local $2)
      )
    )
    (i64.store offset=24
      (get_local $0)
      (i64.load offset=24
        (get_local $2)
      )
    )
    (i64.store offset=32
      (get_local $0)
      (i64.load offset=32
        (get_local $2)
      )
    )
    ;; raybench.cpp:369
    (i64.store offset=40
      (get_local $0)
      (i64.load offset=40
        (get_local $2)
      )
    )
  )
  (func $__ZN6Volume6centerEv (param $0 i32) (param $1 i32)
    ;; raybench.cpp:378
    (call $__Z5CRASHPKc
      (i32.const 2631)
    )
  )
  (func $__ZN6Volume5debugEPFvPKcEj (param $0 i32) (param $1 i32) (param $2 i32)
    (local $3 i32)
    (local $4 i32)
    ;; raybench.cpp:383
    (call_indirect $FUNCSIG$vi
      (i32.const 2665)
      (i32.add
        (i32.and
          (get_local $1)
          (i32.const 15)
        )
        (i32.const 12)
      )
    )
    ;; raybench.cpp:384
    (set_local $4
      (i32.load offset=144
        (get_local $0)
      )
    )
    ;; raybench.cpp:384
    (set_local $3
      (i32.load
        (get_local $4)
      )
    )
    ;; raybench.cpp:384
    (set_local $3
      (i32.load offset=16
        (get_local $3)
      )
    )
    ;; raybench.cpp:384
    (call_indirect $FUNCSIG$viii
      (get_local $4)
      (get_local $1)
      (tee_local $4
        (i32.add
          (get_local $2)
          (i32.const 1)
        )
      )
      (i32.add
        (i32.and
          (get_local $3)
          (i32.const 15)
        )
        (i32.const 56)
      )
    )
    ;; raybench.cpp:385
    (set_local $0
      (i32.load
        (tee_local $3
          (i32.add
            (get_local $0)
            (i32.const 148)
          )
        )
      )
    )
    (if
      (i32.eqz
        (get_local $0)
      )
      (block
        ;; raybench.cpp:391
        (call_indirect $FUNCSIG$vi
          (i32.const 2672)
          (i32.add
            (i32.and
              (get_local $1)
              (i32.const 15)
            )
            (i32.const 12)
          )
        )
        (return)
      )
    )
    ;; raybench.cpp:386
    (call_indirect $FUNCSIG$vi
      (i32.const 2667)
      (i32.add
        (i32.and
          (get_local $1)
          (i32.const 15)
        )
        (i32.const 12)
      )
    )
    (if
      (get_local $2)
      (block
        (set_local $0
          (i32.const 0)
        )
        (loop $while-in
          ;; raybench.cpp:388
          (call_indirect $FUNCSIG$vi
            (i32.const 2670)
            (i32.add
              (i32.and
                (get_local $1)
                (i32.const 15)
              )
              (i32.const 12)
            )
          )
          (br_if $while-in
            (i32.ne
              (tee_local $0
                (i32.add
                  (get_local $0)
                  (i32.const 1)
                )
              )
              (get_local $2)
            )
          )
        )
      )
    )
    ;; raybench.cpp:389
    (set_local $0
      (i32.load
        (get_local $3)
      )
    )
    ;; raybench.cpp:389
    (set_local $2
      (i32.load
        (get_local $0)
      )
    )
    ;; raybench.cpp:389
    (set_local $2
      (i32.load offset=16
        (get_local $2)
      )
    )
    ;; raybench.cpp:389
    (call_indirect $FUNCSIG$viii
      (get_local $0)
      (get_local $1)
      (get_local $4)
      (i32.add
        (i32.and
          (get_local $2)
          (i32.const 15)
        )
        (i32.const 56)
      )
    )
    ;; raybench.cpp:391
    (call_indirect $FUNCSIG$vi
      (i32.const 2672)
      (i32.add
        (i32.and
          (get_local $1)
          (i32.const 15)
        )
        (i32.const 12)
      )
    )
  )
  (func $__ZN6Sphere9intersectERK4Vec3S2_ddPd (param $0 i32) (param $1 i32) (param $2 i32) (param $3 f64) (param $4 f64) (param $5 i32) (result i32)
    (local $6 f64)
    (local $7 f64)
    (local $8 f64)
    (local $9 f64)
    (local $10 f64)
    (local $11 f64)
    (local $12 f64)
    (local $13 f64)
    (local $14 f64)
    (set_local $6
      (f64.load
        (get_local $2)
      )
    )
    (set_local $7
      (f64.load offset=8
        (get_local $2)
      )
    )
    ;; raybench.cpp:208
    (set_local $8
      (f64.load offset=16
        (get_local $2)
      )
    )
    (set_local $9
      (f64.load offset=8
        (get_local $1)
      )
    )
    (set_local $10
      (f64.load offset=16
        (get_local $1)
      )
    )
    (set_local $11
      (f64.load offset=104
        (get_local $0)
      )
    )
    (set_local $14
      (f64.load offset=112
        (get_local $0)
      )
    )
    ;; raybench.cpp:171
    (set_local $12
      (f64.sub
        (f64.load
          (get_local $1)
        )
        (f64.load offset=96
          (get_local $0)
        )
      )
    )
    ;; raybench.cpp:459
    (set_local $13
      (f64.load offset=120
        (get_local $0)
      )
    )
    ;; raybench.cpp:462
    (if
      (f64.lt
        (tee_local $6
          (f64.sub
            (f64.mul
              (tee_local $9
                (f64.add
                  (f64.add
                    (f64.mul
                      (get_local $6)
                      (get_local $12)
                    )
                    (f64.mul
                      (get_local $7)
                      (tee_local $11
                        (f64.sub
                          (get_local $9)
                          (get_local $11)
                        )
                      )
                    )
                  )
                  (f64.mul
                    (get_local $8)
                    (tee_local $10
                      (f64.sub
                        (get_local $10)
                        (get_local $14)
                      )
                    )
                  )
                )
              )
              (get_local $9)
            )
            (f64.mul
              (tee_local $7
                (f64.add
                  (f64.add
                    (f64.mul
                      (get_local $6)
                      (get_local $6)
                    )
                    (f64.mul
                      (get_local $7)
                      (get_local $7)
                    )
                  )
                  (f64.mul
                    (get_local $8)
                    (get_local $8)
                  )
                )
              )
              (f64.sub
                (f64.add
                  (f64.add
                    (f64.mul
                      (get_local $12)
                      (get_local $12)
                    )
                    (f64.mul
                      (get_local $11)
                      (get_local $11)
                    )
                  )
                  (f64.mul
                    (get_local $10)
                    (get_local $10)
                  )
                )
                (f64.mul
                  (get_local $13)
                  (get_local $13)
                )
              )
            )
          )
        )
        (f64.const 0)
      )
      (return
        (i32.const 0)
      )
    )
    ;; raybench.cpp:462
    (set_local $6
      (f64.div
        (f64.sub
          (tee_local $8
            (f64.sqrt
              (get_local $6)
            )
          )
          (get_local $9)
        )
        (get_local $7)
      )
    )
    ;; raybench.cpp:463
    (set_local $7
      (f64.div
        (f64.sub
          (f64.neg
            (get_local $9)
          )
          (get_local $8)
        )
        (get_local $7)
      )
    )
    (if
      (f64.eq
        (if f64
          (f64.lt
            (if f64
              (i32.or
                (f64.lt
                  (get_local $6)
                  (get_local $3)
                )
                (f64.gt
                  (get_local $6)
                  (get_local $4)
                )
              )
              (tee_local $6
                (f64.const 1.e+32)
              )
              (get_local $6)
            )
            (if f64
              (i32.or
                (f64.lt
                  (get_local $7)
                  (get_local $3)
                )
                (f64.gt
                  (get_local $7)
                  (get_local $4)
                )
              )
              (tee_local $7
                (f64.const 1.e+32)
              )
              (get_local $7)
            )
          )
          (get_local $6)
          (tee_local $6
            (get_local $7)
          )
        )
        (f64.const 1.e+32)
      )
      (return
        (i32.const 0)
      )
    )
    ;; raybench.cpp:472
    (f64.store
      (get_local $5)
      (get_local $6)
    )
    (get_local $0)
  )
  (func $__ZN6Sphere6normalERK4Vec3 (param $0 i32) (param $1 i32) (param $2 i32)
    (local $3 f64)
    (local $4 f64)
    (local $5 f64)
    (local $6 f64)
    (local $7 f64)
    (local $8 f64)
    (set_local $4
      (f64.load offset=8
        (get_local $2)
      )
    )
    (set_local $5
      (f64.load offset=16
        (get_local $2)
      )
    )
    (set_local $6
      (f64.load offset=104
        (get_local $1)
      )
    )
    (set_local $7
      (f64.load offset=112
        (get_local $1)
      )
    )
    ;; raybench.cpp:171
    (set_local $8
      (f64.sub
        (f64.load
          (get_local $2)
        )
        (f64.load offset=96
          (get_local $1)
        )
      )
    )
    ;; raybench.cpp:477
    (set_local $3
      (f64.load offset=120
        (get_local $1)
      )
    )
    ;; raybench.cpp:148
    (f64.store
      (get_local $0)
      (f64.div
        (get_local $8)
        (get_local $3)
      )
    )
    ;; raybench.cpp:149
    (f64.store offset=8
      (get_local $0)
      (f64.div
        (f64.sub
          (get_local $4)
          (get_local $6)
        )
        (get_local $3)
      )
    )
    ;; raybench.cpp:150
    (f64.store offset=16
      (get_local $0)
      (f64.div
        (f64.sub
          (get_local $5)
          (get_local $7)
        )
        (get_local $3)
      )
    )
  )
  (func $__ZN6Sphere6boundsEv (param $0 i32) (param $1 i32)
    (local $2 f64)
    (local $3 f64)
    (local $4 f64)
    (local $5 f64)
    ;; raybench.cpp:481
    (set_local $3
      (f64.load offset=96
        (get_local $1)
      )
    )
    ;; raybench.cpp:481
    (set_local $2
      (f64.load offset=120
        (get_local $1)
      )
    )
    ;; raybench.cpp:482
    (set_local $4
      (f64.load offset=104
        (get_local $1)
      )
    )
    ;; raybench.cpp:483
    (set_local $5
      (f64.load offset=112
        (get_local $1)
      )
    )
    ;; raybench.cpp:262
    (f64.store
      (get_local $0)
      (f64.sub
        (get_local $3)
        (get_local $2)
      )
    )
    ;; raybench.cpp:263
    (f64.store offset=8
      (get_local $0)
      (f64.add
        (get_local $3)
        (get_local $2)
      )
    )
    ;; raybench.cpp:264
    (f64.store offset=16
      (get_local $0)
      (f64.sub
        (get_local $4)
        (get_local $2)
      )
    )
    ;; raybench.cpp:265
    (f64.store offset=24
      (get_local $0)
      (f64.add
        (get_local $2)
        (get_local $4)
      )
    )
    ;; raybench.cpp:266
    (f64.store offset=32
      (get_local $0)
      (f64.sub
        (get_local $5)
        (get_local $2)
      )
    )
    ;; raybench.cpp:267
    (f64.store offset=40
      (get_local $0)
      (f64.add
        (get_local $2)
        (get_local $5)
      )
    )
  )
  (func $__ZN6Sphere6centerEv (param $0 i32) (param $1 i32)
    (local $2 i32)
    (i64.store
      (get_local $0)
      (i64.load
        (tee_local $2
          (i32.add
            (get_local $1)
            (i32.const 96)
          )
        )
      )
    )
    (i64.store offset=8
      (get_local $0)
      (i64.load offset=8
        (get_local $2)
      )
    )
    ;; raybench.cpp:487
    (i64.store offset=16
      (get_local $0)
      (i64.load offset=16
        (get_local $2)
      )
    )
  )
  (func $__ZN6Sphere5debugEPFvPKcEj (param $0 i32) (param $1 i32) (param $2 i32)
    (local $3 f64)
    (local $4 f64)
    (local $5 f64)
    (local $6 f64)
    (local $7 i32)
    (set_local $2
      (get_global $STACKTOP)
    )
    ;; raybench.cpp:492
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 288)
      )
    )
    ;; raybench.cpp:492
    (set_local $3
      (f64.load offset=96
        (get_local $0)
      )
    )
    ;; raybench.cpp:492
    (set_local $4
      (f64.load offset=104
        (get_local $0)
      )
    )
    ;; raybench.cpp:492
    (set_local $5
      (f64.load offset=112
        (get_local $0)
      )
    )
    ;; raybench.cpp:492
    (set_local $6
      (f64.load offset=120
        (get_local $0)
      )
    )
    ;; raybench.cpp:492
    (f64.store
      (tee_local $0
        (get_local $2)
      )
      (get_local $3)
    )
    ;; raybench.cpp:492
    (f64.store offset=8
      (get_local $0)
      (get_local $4)
    )
    ;; raybench.cpp:492
    (f64.store offset=16
      (get_local $0)
      (get_local $5)
    )
    ;; raybench.cpp:492
    (f64.store offset=24
      (get_local $0)
      (get_local $6)
    )
    ;; raybench.cpp:492
    (drop
      (call $_sprintf
        (tee_local $7
          (i32.add
            (get_local $2)
            (i32.const 32)
          )
        )
        (i32.const 2682)
        (get_local $0)
      )
    )
    ;; raybench.cpp:493
    (call_indirect $FUNCSIG$vi
      (get_local $7)
      (i32.add
        (i32.and
          (get_local $1)
          (i32.const 15)
        )
        (i32.const 12)
      )
    )
    (set_global $STACKTOP
      (get_local $2)
    )
  )
  (func $__ZNSt3__26vectorIP7SurfaceNS_9allocatorIS2_EEE21__push_back_slow_pathIS2_EEvOT_ (param $0 i32) (param $1 i32)
    (local $2 i32)
    (local $3 i32)
    (local $4 i32)
    (local $5 i32)
    (local $6 i32)
    (local $7 i32)
    (local $8 i32)
    (local $9 i32)
    (local $10 i32)
    (local $11 i32)
    (local $12 i32)
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:640
    (set_local $2
      (i32.load
        (tee_local $6
          (i32.add
            (get_local $0)
            (i32.const 4)
          )
        )
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:640
    (set_local $4
      (i32.load
        (get_local $0)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:2313
    (if
      (i32.gt_u
        (tee_local $3
          (i32.add
            (i32.shr_s
              (i32.sub
                (get_local $2)
                (get_local $4)
              )
              (i32.const 2)
            )
            (i32.const 1)
          )
        )
        (i32.const 1073741823)
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:963
      (call $__ZNKSt3__220__vector_base_commonILb1EE20__throw_length_errorEv
        (get_local $0)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:372
    (set_local $2
      (i32.load
        (tee_local $12
          (i32.add
            (get_local $0)
            (i32.const 8)
          )
        )
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/__split_buffer:312
    (block $__rjto$0
      (block $__rjti$0
        (if
          ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:967
          (i32.lt_u
            (i32.shr_s
              (tee_local $7
                (i32.sub
                  (get_local $2)
                  (get_local $4)
                )
              )
              (i32.const 2)
            )
            (i32.const 536870911)
          )
          (block
            ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:640
            (set_local $2
              (i32.load
                (get_local $6)
              )
            )
            ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:640
            (set_local $5
              (i32.shr_s
                (i32.sub
                  (get_local $2)
                  (get_local $4)
                )
                (i32.const 2)
              )
            )
            (if
              ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1730
              (if i32
                (i32.lt_u
                  (tee_local $7
                    (i32.shr_s
                      (get_local $7)
                      (i32.const 1)
                    )
                  )
                  (get_local $3)
                )
                (get_local $3)
                (tee_local $3
                  (get_local $7)
                )
              )
              (block
                (br_if $__rjti$0
                  (i32.le_u
                    (get_local $3)
                    (i32.const 1073741823)
                  )
                )
                ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1732
                (set_local $2
                  (call $___cxa_allocate_exception
                    (i32.const 4)
                  )
                )
                ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1732
                (call $__ZNSt9bad_allocC2Ev
                  (get_local $2)
                )
                ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1732
                (call $___cxa_throw
                  (get_local $2)
                  (i32.const 1488)
                  (i32.const 6)
                )
              )
              (block
                (set_local $9
                  (i32.const 0)
                )
                (set_local $8
                  (i32.const 0)
                )
                (set_local $10
                  (get_local $5)
                )
                (set_local $11
                  (get_local $2)
                )
              )
            )
          )
          (block
            ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/vector:640
            (set_local $5
              (i32.load
                (get_local $6)
              )
            )
            (set_local $3
              (i32.const 1073741823)
            )
            (set_local $2
              (get_local $5)
            )
            (set_local $5
              (i32.shr_s
                (i32.sub
                  (get_local $5)
                  (get_local $4)
                )
                (i32.const 2)
              )
            )
            (br $__rjti$0)
          )
        )
        (br $__rjto$0)
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/new:168
      (set_local $8
        (call $__Znwj
          (i32.shl
            (get_local $3)
            (i32.const 2)
          )
        )
      )
      (set_local $9
        (get_local $3)
      )
      (set_local $10
        (get_local $5)
      )
      (set_local $11
        (get_local $2)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
    (set_local $2
      (i32.load
        (get_local $1)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1748
    (i32.store
      (tee_local $1
        (i32.add
          (get_local $8)
          (i32.shl
            (get_local $10)
            (i32.const 2)
          )
        )
      )
      (get_local $2)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1636
    (set_local $3
      (i32.add
        (get_local $1)
        (i32.shl
          (i32.sub
            (i32.const 0)
            (i32.shr_s
              (tee_local $2
                (i32.sub
                  (get_local $11)
                  (get_local $4)
                )
              )
              (i32.const 2)
            )
          )
          (i32.const 2)
        )
      )
    )
    (if
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1638
      (i32.gt_s
        (get_local $2)
        (i32.const 0)
      )
      ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1638
      (drop
        (call $_memcpy
          (get_local $3)
          (get_local $4)
          (get_local $2)
        )
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/type_traits:4454
    (i32.store
      (get_local $0)
      (get_local $3)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/type_traits:4454
    (i32.store
      (get_local $6)
      (i32.add
        (get_local $1)
        (i32.const 4)
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/type_traits:4454
    (i32.store
      (get_local $12)
      (i32.add
        (get_local $8)
        (i32.shl
          (get_local $9)
          (i32.const 2)
        )
      )
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/memory:1739
    (if
      (i32.eqz
        (get_local $4)
      )
      (return)
    )
    ;; /Users/lhansen/lib/emsdk_portable/emscripten/incoming/system/include/libcxx/new:176
    (call $__ZdlPv
      (get_local $4)
    )
  )
  (func $___stdio_close (param $0 i32) (result i32)
    (local $1 i32)
    (local $2 i32)
    (set_local $1
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 16)
      )
    )
    (i32.store
      (tee_local $2
        (get_local $1)
      )
      (i32.load offset=60
        (get_local $0)
      )
    )
    (set_local $0
      (call $___syscall_ret
        (call $___syscall6
          (i32.const 6)
          (get_local $2)
        )
      )
    )
    (set_global $STACKTOP
      (get_local $1)
    )
    (get_local $0)
  )
  (func $___stdio_write (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
    (local $3 i32)
    (local $4 i32)
    (local $5 i32)
    (local $6 i32)
    (local $7 i32)
    (local $8 i32)
    (local $9 i32)
    (local $10 i32)
    (local $11 i32)
    (local $12 i32)
    (local $13 i32)
    (local $14 i32)
    (local $15 i32)
    (set_local $7
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 48)
      )
    )
    (set_local $8
      (i32.add
        (get_local $7)
        (i32.const 16)
      )
    )
    (set_local $9
      (get_local $7)
    )
    (i32.store
      (tee_local $3
        (i32.add
          (get_local $7)
          (i32.const 32)
        )
      )
      (tee_local $4
        (i32.load
          (tee_local $6
            (i32.add
              (get_local $0)
              (i32.const 28)
            )
          )
        )
      )
    )
    (i32.store offset=4
      (get_local $3)
      (tee_local $4
        (i32.sub
          (i32.load
            (tee_local $11
              (i32.add
                (get_local $0)
                (i32.const 20)
              )
            )
          )
          (get_local $4)
        )
      )
    )
    (i32.store offset=8
      (get_local $3)
      (get_local $1)
    )
    (i32.store offset=12
      (get_local $3)
      (get_local $2)
    )
    (set_local $14
      (i32.add
        (get_local $0)
        (i32.const 60)
      )
    )
    (set_local $15
      (i32.add
        (get_local $0)
        (i32.const 44)
      )
    )
    (set_local $5
      (i32.const 2)
    )
    (set_local $12
      (i32.add
        (get_local $4)
        (get_local $2)
      )
    )
    (set_local $1
      (get_local $3)
    )
    (block $__rjto$1
      (block $__rjti$1
        (block $__rjti$0
          (loop $while-in
            (if
              (i32.load
                (i32.const 5496)
              )
              (block
                (call $_pthread_cleanup_push
                  (i32.const 8)
                  (get_local $0)
                )
                (i32.store
                  (get_local $9)
                  (i32.load
                    (get_local $14)
                  )
                )
                (i32.store offset=4
                  (get_local $9)
                  (get_local $1)
                )
                (i32.store offset=8
                  (get_local $9)
                  (get_local $5)
                )
                (set_local $4
                  (call $___syscall_ret
                    (call $___syscall146
                      (i32.const 146)
                      (get_local $9)
                    )
                  )
                )
                (call $_pthread_cleanup_pop
                  (i32.const 0)
                )
              )
              (block
                (i32.store
                  (get_local $8)
                  (i32.load
                    (get_local $14)
                  )
                )
                (i32.store offset=4
                  (get_local $8)
                  (get_local $1)
                )
                (i32.store offset=8
                  (get_local $8)
                  (get_local $5)
                )
                (set_local $4
                  (call $___syscall_ret
                    (call $___syscall146
                      (i32.const 146)
                      (get_local $8)
                    )
                  )
                )
              )
            )
            (br_if $__rjti$0
              (i32.eq
                (get_local $12)
                (get_local $4)
              )
            )
            (br_if $__rjti$1
              (i32.lt_s
                (get_local $4)
                (i32.const 0)
              )
            )
            (set_local $1
              (if i32
                (i32.gt_u
                  (get_local $4)
                  (tee_local $13
                    (i32.load offset=4
                      (get_local $1)
                    )
                  )
                )
                (block i32
                  (i32.store
                    (get_local $6)
                    (tee_local $3
                      (i32.load
                        (get_local $15)
                      )
                    )
                  )
                  (i32.store
                    (get_local $11)
                    (get_local $3)
                  )
                  (set_local $10
                    (i32.sub
                      (get_local $4)
                      (get_local $13)
                    )
                  )
                  (set_local $5
                    (i32.add
                      (get_local $5)
                      (i32.const -1)
                    )
                  )
                  (set_local $3
                    (i32.add
                      (get_local $1)
                      (i32.const 8)
                    )
                  )
                  (i32.load offset=12
                    (get_local $1)
                  )
                )
                (if i32
                  (i32.eq
                    (get_local $5)
                    (i32.const 2)
                  )
                  (block i32
                    (i32.store
                      (get_local $6)
                      (i32.add
                        (i32.load
                          (get_local $6)
                        )
                        (get_local $4)
                      )
                    )
                    (set_local $10
                      (get_local $4)
                    )
                    (set_local $5
                      (i32.const 2)
                    )
                    (set_local $3
                      (get_local $1)
                    )
                    (get_local $13)
                  )
                  (block i32
                    (set_local $10
                      (get_local $4)
                    )
                    (set_local $3
                      (get_local $1)
                    )
                    (get_local $13)
                  )
                )
              )
            )
            (i32.store
              (get_local $3)
              (i32.add
                (i32.load
                  (get_local $3)
                )
                (get_local $10)
              )
            )
            (i32.store offset=4
              (get_local $3)
              (i32.sub
                (get_local $1)
                (get_local $10)
              )
            )
            (set_local $12
              (i32.sub
                (get_local $12)
                (get_local $4)
              )
            )
            (set_local $1
              (get_local $3)
            )
            (br $while-in)
          )
        )
        (i32.store offset=16
          (get_local $0)
          (i32.add
            (tee_local $1
              (i32.load
                (get_local $15)
              )
            )
            (i32.load offset=48
              (get_local $0)
            )
          )
        )
        (i32.store
          (get_local $6)
          (tee_local $0
            (get_local $1)
          )
        )
        (i32.store
          (get_local $11)
          (get_local $0)
        )
        (br $__rjto$1)
      )
      (i32.store offset=16
        (get_local $0)
        (i32.const 0)
      )
      (i32.store
        (get_local $6)
        (i32.const 0)
      )
      (i32.store
        (get_local $11)
        (i32.const 0)
      )
      (i32.store
        (get_local $0)
        (i32.or
          (i32.load
            (get_local $0)
          )
          (i32.const 32)
        )
      )
      (set_local $2
        (if i32
          (i32.eq
            (get_local $5)
            (i32.const 2)
          )
          (i32.const 0)
          (i32.sub
            (get_local $2)
            (i32.load offset=4
              (get_local $1)
            )
          )
        )
      )
    )
    (set_global $STACKTOP
      (get_local $7)
    )
    (get_local $2)
  )
  (func $___stdio_seek (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
    (local $3 i32)
    (local $4 i32)
    (set_local $4
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 32)
      )
    )
    (i32.store
      (tee_local $3
        (get_local $4)
      )
      (i32.load offset=60
        (get_local $0)
      )
    )
    (i32.store offset=4
      (get_local $3)
      (i32.const 0)
    )
    (i32.store offset=8
      (get_local $3)
      (get_local $1)
    )
    (i32.store offset=12
      (get_local $3)
      (tee_local $0
        (i32.add
          (get_local $4)
          (i32.const 20)
        )
      )
    )
    (i32.store offset=16
      (get_local $3)
      (get_local $2)
    )
    (set_local $0
      (if i32
        (i32.lt_s
          (call $___syscall_ret
            (call $___syscall140
              (i32.const 140)
              (get_local $3)
            )
          )
          (i32.const 0)
        )
        (block i32
          (i32.store
            (get_local $0)
            (i32.const -1)
          )
          (i32.const -1)
        )
        (i32.load
          (get_local $0)
        )
      )
    )
    (set_global $STACKTOP
      (get_local $4)
    )
    (get_local $0)
  )
  (func $___syscall_ret (param $0 i32) (result i32)
    (if i32
      (i32.gt_u
        (get_local $0)
        (i32.const -4096)
      )
      (block i32
        (i32.store
          (call $___errno_location)
          (i32.sub
            (i32.const 0)
            (get_local $0)
          )
        )
        (i32.const -1)
      )
      (get_local $0)
    )
  )
  (func $___errno_location (result i32)
    (if i32
      (i32.load
        (i32.const 5496)
      )
      (i32.load offset=64
        (call $_pthread_self)
      )
      (i32.const 5540)
    )
  )
  (func $_cleanup_387 (param $0 i32)
    (if
      (i32.eqz
        (i32.load offset=68
          (get_local $0)
        )
      )
      (call $___unlockfile
        (get_local $0)
      )
    )
  )
  (func $___unlockfile (param $0 i32)
    (nop)
  )
  (func $___stdout_write (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
    (local $3 i32)
    (local $4 i32)
    (local $5 i32)
    (set_local $4
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 80)
      )
    )
    (set_local $3
      (get_local $4)
    )
    (set_local $5
      (i32.add
        (get_local $4)
        (i32.const 12)
      )
    )
    (i32.store offset=36
      (get_local $0)
      (i32.const 1)
    )
    (if
      (i32.eqz
        (i32.and
          (i32.load
            (get_local $0)
          )
          (i32.const 64)
        )
      )
      (block
        (i32.store
          (get_local $3)
          (i32.load offset=60
            (get_local $0)
          )
        )
        (i32.store offset=4
          (get_local $3)
          (i32.const 21505)
        )
        (i32.store offset=8
          (get_local $3)
          (get_local $5)
        )
        (if
          (call $___syscall54
            (i32.const 54)
            (get_local $3)
          )
          (i32.store8 offset=75
            (get_local $0)
            (i32.const -1)
          )
        )
      )
    )
    (set_local $0
      (call $___stdio_write
        (get_local $0)
        (get_local $1)
        (get_local $2)
      )
    )
    (set_global $STACKTOP
      (get_local $4)
    )
    (get_local $0)
  )
  (func $_sprintf (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
    (local $3 i32)
    (local $4 i32)
    (set_local $3
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 16)
      )
    )
    (i32.store
      (tee_local $4
        (get_local $3)
      )
      (get_local $2)
    )
    (set_local $0
      (call $_vsprintf
        (get_local $0)
        (get_local $1)
        (get_local $4)
      )
    )
    (set_global $STACKTOP
      (get_local $3)
    )
    (get_local $0)
  )
  (func $_vsprintf (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
    (call $_vsnprintf
      (get_local $0)
      (i32.const 2147483647)
      (get_local $1)
      (get_local $2)
    )
  )
  (func $_vsnprintf (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (result i32)
    (local $4 i32)
    (local $5 i32)
    (local $6 i32)
    (local $7 i32)
    (set_local $6
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 128)
      )
    )
    (set_local $5
      (i32.add
        (get_local $6)
        (i32.const 112)
      )
    )
    (i64.store align=4
      (tee_local $4
        (get_local $6)
      )
      (i64.load align=4
        (i32.const 1848)
      )
    )
    (i64.store offset=8 align=4
      (get_local $4)
      (i64.load align=4
        (i32.const 1856)
      )
    )
    (i64.store offset=16 align=4
      (get_local $4)
      (i64.load align=4
        (i32.const 1864)
      )
    )
    (i64.store offset=24 align=4
      (get_local $4)
      (i64.load align=4
        (i32.const 1872)
      )
    )
    (i64.store offset=32 align=4
      (get_local $4)
      (i64.load align=4
        (i32.const 1880)
      )
    )
    (i64.store offset=40 align=4
      (get_local $4)
      (i64.load align=4
        (i32.const 1888)
      )
    )
    (i64.store offset=48 align=4
      (get_local $4)
      (i64.load align=4
        (i32.const 1896)
      )
    )
    (i64.store offset=56 align=4
      (get_local $4)
      (i64.load align=4
        (i32.const 1904)
      )
    )
    (i64.store offset=64 align=4
      (get_local $4)
      (i64.load align=4
        (i32.const 1912)
      )
    )
    (i64.store offset=72 align=4
      (get_local $4)
      (i64.load align=4
        (i32.const 1920)
      )
    )
    (i64.store offset=80 align=4
      (get_local $4)
      (i64.load align=4
        (i32.const 1928)
      )
    )
    (i64.store offset=88 align=4
      (get_local $4)
      (i64.load align=4
        (i32.const 1936)
      )
    )
    (i64.store offset=96 align=4
      (get_local $4)
      (i64.load align=4
        (i32.const 1944)
      )
    )
    (i64.store offset=104 align=4
      (get_local $4)
      (i64.load align=4
        (i32.const 1952)
      )
    )
    (block $__rjto$0
      (block $__rjti$0
        (if
          (i32.gt_u
            (i32.add
              (get_local $1)
              (i32.const -1)
            )
            (i32.const 2147483646)
          )
          (if
            (get_local $1)
            (block
              (i32.store
                (call $___errno_location)
                (i32.const 75)
              )
              (set_local $0
                (i32.const -1)
              )
            )
            (block
              (set_local $0
                (get_local $5)
              )
              (set_local $5
                (i32.const 1)
              )
              (br $__rjti$0)
            )
          )
          (block
            (set_local $5
              (get_local $1)
            )
            (br $__rjti$0)
          )
        )
        (br $__rjto$0)
      )
      (i32.store offset=48
        (get_local $4)
        (if i32
          (i32.gt_u
            (get_local $5)
            (tee_local $1
              (i32.sub
                (i32.const -2)
                (get_local $0)
              )
            )
          )
          (get_local $1)
          (tee_local $1
            (get_local $5)
          )
        )
      )
      (i32.store
        (tee_local $7
          (i32.add
            (get_local $4)
            (i32.const 20)
          )
        )
        (get_local $0)
      )
      (i32.store offset=44
        (get_local $4)
        (get_local $0)
      )
      (i32.store
        (tee_local $5
          (i32.add
            (get_local $4)
            (i32.const 16)
          )
        )
        (tee_local $0
          (i32.add
            (get_local $0)
            (get_local $1)
          )
        )
      )
      (i32.store offset=28
        (get_local $4)
        (get_local $0)
      )
      (set_local $0
        (call $_vfprintf
          (get_local $4)
          (get_local $2)
          (get_local $3)
        )
      )
      (if
        (get_local $1)
        (i32.store8
          (i32.add
            (tee_local $1
              (i32.load
                (get_local $7)
              )
            )
            (i32.shr_s
              (i32.shl
                (i32.eq
                  (get_local $1)
                  (i32.load
                    (get_local $5)
                  )
                )
                (i32.const 31)
              )
              (i32.const 31)
            )
          )
          (i32.const 0)
        )
      )
    )
    (set_global $STACKTOP
      (get_local $6)
    )
    (get_local $0)
  )
  (func $_vfprintf (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
    (local $3 i32)
    (local $4 i32)
    (local $5 i32)
    (local $6 i32)
    (local $7 i32)
    (local $8 i32)
    (local $9 i32)
    (local $10 i32)
    (local $11 i32)
    (local $12 i32)
    (local $13 i32)
    (local $14 i32)
    (set_local $4
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 224)
      )
    )
    (set_local $5
      (i32.add
        (get_local $4)
        (i32.const 136)
      )
    )
    (i64.store align=4
      (tee_local $3
        (i32.add
          (get_local $4)
          (i32.const 80)
        )
      )
      (i64.const 0)
    )
    (i64.store offset=8 align=4
      (get_local $3)
      (i64.const 0)
    )
    (i64.store offset=16 align=4
      (get_local $3)
      (i64.const 0)
    )
    (i64.store offset=24 align=4
      (get_local $3)
      (i64.const 0)
    )
    (i64.store offset=32 align=4
      (get_local $3)
      (i64.const 0)
    )
    (i32.store
      (tee_local $6
        (i32.add
          (get_local $4)
          (i32.const 120)
        )
      )
      (i32.load
        (get_local $2)
      )
    )
    (if
      (i32.lt_s
        (call $_printf_core
          (i32.const 0)
          (get_local $1)
          (get_local $6)
          (tee_local $2
            (get_local $4)
          )
          (get_local $3)
        )
        (i32.const 0)
      )
      (set_local $1
        (i32.const -1)
      )
      (block
        (set_local $12
          (if i32
            (i32.gt_s
              (i32.load offset=76
                (get_local $0)
              )
              (i32.const -1)
            )
            (call $___lockfile
              (get_local $0)
            )
            (i32.const 0)
          )
        )
        (set_local $7
          (i32.load
            (get_local $0)
          )
        )
        (if
          (i32.lt_s
            (i32.load8_s offset=74
              (get_local $0)
            )
            (i32.const 1)
          )
          (i32.store
            (get_local $0)
            (i32.and
              (get_local $7)
              (i32.const -33)
            )
          )
        )
        (if
          (i32.load
            (tee_local $8
              (i32.add
                (get_local $0)
                (i32.const 48)
              )
            )
          )
          (set_local $1
            (call $_printf_core
              (get_local $0)
              (get_local $1)
              (get_local $6)
              (get_local $2)
              (get_local $3)
            )
          )
          (block
            (set_local $10
              (i32.load
                (tee_local $9
                  (i32.add
                    (get_local $0)
                    (i32.const 44)
                  )
                )
              )
            )
            (i32.store
              (get_local $9)
              (get_local $5)
            )
            (i32.store
              (tee_local $13
                (i32.add
                  (get_local $0)
                  (i32.const 28)
                )
              )
              (get_local $5)
            )
            (i32.store
              (tee_local $11
                (i32.add
                  (get_local $0)
                  (i32.const 20)
                )
              )
              (get_local $5)
            )
            (i32.store
              (get_local $8)
              (i32.const 80)
            )
            (i32.store
              (tee_local $14
                (i32.add
                  (get_local $0)
                  (i32.const 16)
                )
              )
              (i32.add
                (get_local $5)
                (i32.const 80)
              )
            )
            (set_local $1
              (call $_printf_core
                (get_local $0)
                (get_local $1)
                (get_local $6)
                (get_local $2)
                (get_local $3)
              )
            )
            (if
              (get_local $10)
              (block
                (drop
                  (call_indirect $FUNCSIG$iiii
                    (get_local $0)
                    (i32.const 0)
                    (i32.const 0)
                    (i32.add
                      (i32.and
                        (i32.load offset=36
                          (get_local $0)
                        )
                        (i32.const 7)
                      )
                      (i32.const 0)
                    )
                  )
                )
                (if
                  (i32.eqz
                    (i32.load
                      (get_local $11)
                    )
                  )
                  (set_local $1
                    (i32.const -1)
                  )
                )
                (i32.store
                  (get_local $9)
                  (get_local $10)
                )
                (i32.store
                  (get_local $8)
                  (i32.const 0)
                )
                (i32.store
                  (get_local $14)
                  (i32.const 0)
                )
                (i32.store
                  (get_local $13)
                  (i32.const 0)
                )
                (i32.store
                  (get_local $11)
                  (i32.const 0)
                )
              )
            )
          )
        )
        (i32.store
          (get_local $0)
          (i32.or
            (tee_local $2
              (i32.load
                (get_local $0)
              )
            )
            (i32.and
              (get_local $7)
              (i32.const 32)
            )
          )
        )
        (if
          (get_local $12)
          (call $___unlockfile
            (get_local $0)
          )
        )
        (if
          (i32.and
            (get_local $2)
            (i32.const 32)
          )
          (set_local $1
            (i32.const -1)
          )
        )
      )
    )
    (set_global $STACKTOP
      (get_local $4)
    )
    (get_local $1)
  )
  (func $i64u-rem (param $0 i64) (param $1 i64) (result i64)
    (if i64
      (i64.eqz
        (get_local $1)
      )
      (i64.const 0)
      (i64.rem_u
        (get_local $0)
        (get_local $1)
      )
    )
  )
  (func $i64u-div (param $0 i64) (param $1 i64) (result i64)
    (if i64
      (i64.eqz
        (get_local $1)
      )
      (i64.const 0)
      (i64.div_u
        (get_local $0)
        (get_local $1)
      )
    )
  )
  (func $_printf_core (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32) (result i32)
    (local $5 i32)
    (local $6 i32)
    (local $7 i32)
    (local $8 i32)
    (local $9 i32)
    (local $10 i32)
    (local $11 i32)
    (local $12 i32)
    (local $13 i32)
    (local $14 f64)
    (local $15 i32)
    (local $16 i32)
    (local $17 i32)
    (local $18 i32)
    (local $19 i32)
    (local $20 i64)
    (local $21 i32)
    (local $22 i32)
    (local $23 i32)
    (local $24 i32)
    (local $25 i32)
    (local $26 f64)
    (local $27 i32)
    (local $28 i32)
    (local $29 i32)
    (local $30 i32)
    (local $31 i32)
    (local $32 i32)
    (local $33 i32)
    (local $34 i32)
    (local $35 i32)
    (local $36 i32)
    (local $37 i32)
    (local $38 i32)
    (local $39 i32)
    (local $40 i32)
    (local $41 f64)
    (local $42 i32)
    (local $43 i32)
    (local $44 i32)
    (local $45 i32)
    (local $46 i32)
    (local $47 i32)
    (local $48 i32)
    (local $49 i32)
    (local $50 i32)
    (local $51 i32)
    (local $52 i32)
    (local $53 i64)
    (local $54 i32)
    (local $55 i32)
    (set_local $27
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 624)
      )
    )
    (set_local $23
      (i32.add
        (get_local $27)
        (i32.const 16)
      )
    )
    (set_local $16
      (get_local $27)
    )
    (set_local $38
      (i32.add
        (get_local $27)
        (i32.const 528)
      )
    )
    (set_local $34
      (i32.ne
        (get_local $0)
        (i32.const 0)
      )
    )
    (set_local $42
      (tee_local $25
        (i32.add
          (tee_local $17
            (i32.add
              (get_local $27)
              (i32.const 536)
            )
          )
          (i32.const 40)
        )
      )
    )
    (set_local $43
      (i32.add
        (get_local $17)
        (i32.const 39)
      )
    )
    (set_local $46
      (i32.add
        (tee_local $39
          (i32.add
            (get_local $27)
            (i32.const 8)
          )
        )
        (i32.const 4)
      )
    )
    (set_local $47
      (i32.sub
        (i32.const 0)
        (tee_local $30
          (tee_local $22
            (i32.add
              (get_local $27)
              (i32.const 588)
            )
          )
        )
      )
    )
    (set_local $36
      (i32.add
        (tee_local $17
          (i32.add
            (get_local $27)
            (i32.const 576)
          )
        )
        (i32.const 12)
      )
    )
    (set_local $44
      (i32.add
        (get_local $17)
        (i32.const 11)
      )
    )
    (set_local $48
      (i32.sub
        (tee_local $31
          (get_local $36)
        )
        (get_local $30)
      )
    )
    (set_local $49
      (i32.sub
        (i32.const -2)
        (get_local $30)
      )
    )
    (set_local $50
      (i32.add
        (get_local $31)
        (i32.const 2)
      )
    )
    (set_local $52
      (i32.add
        (tee_local $51
          (i32.add
            (get_local $27)
            (i32.const 24)
          )
        )
        (i32.const 288)
      )
    )
    (set_local $45
      (tee_local $35
        (i32.add
          (get_local $22)
          (i32.const 9)
        )
      )
    )
    (set_local $37
      (i32.add
        (get_local $22)
        (i32.const 8)
      )
    )
    (set_local $10
      (i32.const 0)
    )
    (set_local $15
      (i32.const 0)
    )
    (set_local $17
      (i32.const 0)
    )
    (block $label$break$L345
      (block $__rjti$9
        (loop $label$continue$L1
          (block $label$break$L1
            (if
              (i32.gt_s
                (get_local $15)
                (i32.const -1)
              )
              (set_local $15
                (if i32
                  (i32.gt_s
                    (get_local $10)
                    (i32.sub
                      (i32.const 2147483647)
                      (get_local $15)
                    )
                  )
                  (block i32
                    (i32.store
                      (call $___errno_location)
                      (i32.const 75)
                    )
                    (i32.const -1)
                  )
                  (i32.add
                    (get_local $10)
                    (get_local $15)
                  )
                )
              )
            )
            (br_if $__rjti$9
              (i32.eqz
                (i32.shr_s
                  (i32.shl
                    (tee_local $5
                      (i32.load8_s
                        (get_local $1)
                      )
                    )
                    (i32.const 24)
                  )
                  (i32.const 24)
                )
              )
            )
            (set_local $11
              (get_local $1)
            )
            (block $label$break$L12
              (block $__rjti$1
                (loop $label$continue$L9
                  (block $label$break$L9
                    (block $switch-default
                      (block $switch-case0
                        (block $switch-case
                          (br_table $switch-case0 $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-case $switch-default
                            (i32.sub
                              (i32.shr_s
                                (i32.shl
                                  (get_local $5)
                                  (i32.const 24)
                                )
                                (i32.const 24)
                              )
                              (i32.const 0)
                            )
                          )
                        )
                        (set_local $5
                          (get_local $11)
                        )
                        (br $__rjti$1)
                      )
                      (set_local $5
                        (get_local $11)
                      )
                      (br $label$break$L9)
                    )
                    (set_local $11
                      (tee_local $5
                        (i32.add
                          (get_local $11)
                          (i32.const 1)
                        )
                      )
                    )
                    (set_local $5
                      (i32.load8_s
                        (get_local $5)
                      )
                    )
                    (br $label$continue$L9)
                  )
                )
                (br $label$break$L12)
              )
              (loop $while-in
                (br_if $label$break$L12
                  (i32.ne
                    (i32.load8_s offset=1
                      (get_local $11)
                    )
                    (i32.const 37)
                  )
                )
                (set_local $5
                  (i32.add
                    (get_local $5)
                    (i32.const 1)
                  )
                )
                (br_if $while-in
                  (i32.eq
                    (i32.load8_s
                      (tee_local $11
                        (i32.add
                          (get_local $11)
                          (i32.const 2)
                        )
                      )
                    )
                    (i32.const 37)
                  )
                )
              )
            )
            (set_local $10
              (i32.sub
                (get_local $5)
                (get_local $1)
              )
            )
            (if
              (get_local $34)
              (if
                (i32.eqz
                  (i32.and
                    (i32.load
                      (get_local $0)
                    )
                    (i32.const 32)
                  )
                )
                (drop
                  (call $___fwritex
                    (get_local $1)
                    (get_local $10)
                    (get_local $0)
                  )
                )
              )
            )
            (if
              (get_local $10)
              (block
                (set_local $1
                  (get_local $11)
                )
                (br $label$continue$L1)
              )
            )
            (set_local $8
              (if i32
                (i32.lt_u
                  (tee_local $9
                    (i32.add
                      (i32.shr_s
                        (i32.shl
                          (tee_local $5
                            (i32.load8_s
                              (tee_local $10
                                (i32.add
                                  (get_local $11)
                                  (i32.const 1)
                                )
                              )
                            )
                          )
                          (i32.const 24)
                        )
                        (i32.const 24)
                      )
                      (i32.const -48)
                    )
                  )
                  (i32.const 10)
                )
                (block i32
                  (set_local $5
                    (i32.add
                      (get_local $11)
                      (i32.const 3)
                    )
                  )
                  (set_local $11
                    (if i32
                      (tee_local $12
                        (i32.eq
                          (i32.load8_s offset=2
                            (get_local $11)
                          )
                          (i32.const 36)
                        )
                      )
                      (get_local $5)
                      (get_local $10)
                    )
                  )
                  (if
                    (get_local $12)
                    (set_local $17
                      (i32.const 1)
                    )
                  )
                  (if
                    (i32.eqz
                      (get_local $12)
                    )
                    (set_local $9
                      (i32.const -1)
                    )
                  )
                  (set_local $5
                    (i32.load8_s
                      (get_local $11)
                    )
                  )
                  (get_local $17)
                )
                (block i32
                  (set_local $9
                    (i32.const -1)
                  )
                  (set_local $11
                    (get_local $10)
                  )
                  (get_local $17)
                )
              )
            )
            (block $label$break$L25
              (if
                (i32.lt_u
                  (tee_local $10
                    (i32.add
                      (i32.shr_s
                        (i32.shl
                          (get_local $5)
                          (i32.const 24)
                        )
                        (i32.const 24)
                      )
                      (i32.const -32)
                    )
                  )
                  (i32.const 32)
                )
                (block
                  (set_local $17
                    (i32.const 0)
                  )
                  (loop $while-in4
                    (br_if $label$break$L25
                      (i32.eqz
                        (i32.and
                          (i32.shl
                            (i32.const 1)
                            (get_local $10)
                          )
                          (i32.const 75913)
                        )
                      )
                    )
                    (set_local $17
                      (i32.or
                        (i32.shl
                          (i32.const 1)
                          (i32.add
                            (i32.shr_s
                              (i32.shl
                                (get_local $5)
                                (i32.const 24)
                              )
                              (i32.const 24)
                            )
                            (i32.const -32)
                          )
                        )
                        (get_local $17)
                      )
                    )
                    (br_if $while-in4
                      (i32.lt_u
                        (tee_local $10
                          (i32.add
                            (i32.shr_s
                              (i32.shl
                                (tee_local $5
                                  (i32.load8_s
                                    (tee_local $11
                                      (i32.add
                                        (get_local $11)
                                        (i32.const 1)
                                      )
                                    )
                                  )
                                )
                                (i32.const 24)
                              )
                              (i32.const 24)
                            )
                            (i32.const -32)
                          )
                        )
                        (i32.const 32)
                      )
                    )
                  )
                )
                (set_local $17
                  (i32.const 0)
                )
              )
            )
            (block $do-once5
              (if
                (i32.eq
                  (i32.shr_s
                    (i32.shl
                      (get_local $5)
                      (i32.const 24)
                    )
                    (i32.const 24)
                  )
                  (i32.const 42)
                )
                (block
                  (set_local $8
                    (block $__rjto$0 i32
                      (block $__rjti$0
                        (br_if $__rjti$0
                          (i32.ge_u
                            (tee_local $10
                              (i32.add
                                (i32.shr_s
                                  (i32.shl
                                    (tee_local $6
                                      (i32.load8_s
                                        (tee_local $5
                                          (i32.add
                                            (get_local $11)
                                            (i32.const 1)
                                          )
                                        )
                                      )
                                    )
                                    (i32.const 24)
                                  )
                                  (i32.const 24)
                                )
                                (i32.const -48)
                              )
                            )
                            (i32.const 10)
                          )
                        )
                        (br_if $__rjti$0
                          (i32.ne
                            (i32.load8_s offset=2
                              (get_local $11)
                            )
                            (i32.const 36)
                          )
                        )
                        (i32.store
                          (i32.add
                            (get_local $4)
                            (i32.shl
                              (get_local $10)
                              (i32.const 2)
                            )
                          )
                          (i32.const 10)
                        )
                        (set_local $10
                          (i32.wrap/i64
                            (i64.load
                              (i32.add
                                (get_local $3)
                                (i32.shl
                                  (i32.add
                                    (i32.load8_s
                                      (get_local $5)
                                    )
                                    (i32.const -48)
                                  )
                                  (i32.const 3)
                                )
                              )
                            )
                          )
                        )
                        (set_local $5
                          (i32.add
                            (get_local $11)
                            (i32.const 3)
                          )
                        )
                        (br $__rjto$0
                          (i32.const 1)
                        )
                      )
                      (if
                        (get_local $8)
                        (block
                          (set_local $15
                            (i32.const -1)
                          )
                          (br $label$break$L1)
                        )
                      )
                      (if
                        (i32.eqz
                          (get_local $34)
                        )
                        (block
                          (set_local $10
                            (i32.const 0)
                          )
                          (set_local $12
                            (get_local $17)
                          )
                          (set_local $17
                            (i32.const 0)
                          )
                          (set_local $11
                            (get_local $5)
                          )
                          (set_local $5
                            (get_local $6)
                          )
                          (br $do-once5)
                        )
                      )
                      (set_local $10
                        (i32.load
                          (tee_local $11
                            (i32.and
                              (i32.add
                                (i32.load
                                  (get_local $2)
                                )
                                (i32.const 3)
                              )
                              (i32.const -4)
                            )
                          )
                        )
                      )
                      (i32.store
                        (get_local $2)
                        (i32.add
                          (get_local $11)
                          (i32.const 4)
                        )
                      )
                      (i32.const 0)
                    )
                  )
                  (set_local $12
                    (i32.or
                      (get_local $17)
                      (i32.const 8192)
                    )
                  )
                  (set_local $11
                    (i32.sub
                      (i32.const 0)
                      (get_local $10)
                    )
                  )
                  (if
                    (i32.eqz
                      (tee_local $6
                        (i32.lt_s
                          (get_local $10)
                          (i32.const 0)
                        )
                      )
                    )
                    (set_local $12
                      (get_local $17)
                    )
                  )
                  (if
                    (get_local $6)
                    (set_local $10
                      (get_local $11)
                    )
                  )
                  (set_local $17
                    (get_local $8)
                  )
                  (set_local $11
                    (get_local $5)
                  )
                  (set_local $5
                    (i32.load8_s
                      (get_local $5)
                    )
                  )
                )
                (if
                  (i32.lt_u
                    (tee_local $12
                      (i32.add
                        (i32.shr_s
                          (i32.shl
                            (get_local $5)
                            (i32.const 24)
                          )
                          (i32.const 24)
                        )
                        (i32.const -48)
                      )
                    )
                    (i32.const 10)
                  )
                  (block
                    (set_local $10
                      (i32.const 0)
                    )
                    (set_local $5
                      (get_local $12)
                    )
                    (loop $while-in8
                      (set_local $10
                        (i32.add
                          (i32.mul
                            (get_local $10)
                            (i32.const 10)
                          )
                          (get_local $5)
                        )
                      )
                      (br_if $while-in8
                        (i32.lt_u
                          (tee_local $5
                            (i32.add
                              (i32.shr_s
                                (i32.shl
                                  (tee_local $6
                                    (i32.load8_s
                                      (tee_local $11
                                        (i32.add
                                          (get_local $11)
                                          (i32.const 1)
                                        )
                                      )
                                    )
                                  )
                                  (i32.const 24)
                                )
                                (i32.const 24)
                              )
                              (i32.const -48)
                            )
                          )
                          (i32.const 10)
                        )
                      )
                    )
                    (if
                      (i32.lt_s
                        (get_local $10)
                        (i32.const 0)
                      )
                      (block
                        (set_local $15
                          (i32.const -1)
                        )
                        (br $label$break$L1)
                      )
                      (block
                        (set_local $12
                          (get_local $17)
                        )
                        (set_local $17
                          (get_local $8)
                        )
                        (set_local $5
                          (get_local $6)
                        )
                      )
                    )
                  )
                  (block
                    (set_local $10
                      (i32.const 0)
                    )
                    (set_local $12
                      (get_local $17)
                    )
                    (set_local $17
                      (get_local $8)
                    )
                  )
                )
              )
            )
            (block $label$break$L45
              (if
                (i32.eq
                  (i32.shr_s
                    (i32.shl
                      (get_local $5)
                      (i32.const 24)
                    )
                    (i32.const 24)
                  )
                  (i32.const 46)
                )
                (block
                  (if
                    (i32.ne
                      (i32.shr_s
                        (i32.shl
                          (tee_local $5
                            (i32.load8_s
                              (tee_local $8
                                (i32.add
                                  (get_local $11)
                                  (i32.const 1)
                                )
                              )
                            )
                          )
                          (i32.const 24)
                        )
                        (i32.const 24)
                      )
                      (i32.const 42)
                    )
                    (block
                      (if
                        (i32.lt_u
                          (tee_local $5
                            (i32.add
                              (i32.shr_s
                                (i32.shl
                                  (get_local $5)
                                  (i32.const 24)
                                )
                                (i32.const 24)
                              )
                              (i32.const -48)
                            )
                          )
                          (i32.const 10)
                        )
                        (block
                          (set_local $6
                            (i32.const 0)
                          )
                          (set_local $11
                            (get_local $8)
                          )
                        )
                        (block
                          (set_local $5
                            (i32.const 0)
                          )
                          (set_local $11
                            (get_local $8)
                          )
                          (br $label$break$L45)
                        )
                      )
                      (loop $while-in11
                        (set_local $5
                          (i32.add
                            (i32.mul
                              (get_local $6)
                              (i32.const 10)
                            )
                            (get_local $5)
                          )
                        )
                        (br_if $label$break$L45
                          (i32.ge_u
                            (tee_local $8
                              (i32.add
                                (i32.load8_s
                                  (tee_local $11
                                    (i32.add
                                      (get_local $11)
                                      (i32.const 1)
                                    )
                                  )
                                )
                                (i32.const -48)
                              )
                            )
                            (i32.const 10)
                          )
                        )
                        (set_local $6
                          (get_local $5)
                        )
                        (set_local $5
                          (get_local $8)
                        )
                        (br $while-in11)
                      )
                    )
                  )
                  (if
                    (i32.lt_u
                      (tee_local $5
                        (i32.add
                          (i32.load8_s
                            (tee_local $8
                              (i32.add
                                (get_local $11)
                                (i32.const 2)
                              )
                            )
                          )
                          (i32.const -48)
                        )
                      )
                      (i32.const 10)
                    )
                    (if
                      (i32.eq
                        (i32.load8_s offset=3
                          (get_local $11)
                        )
                        (i32.const 36)
                      )
                      (block
                        (i32.store
                          (i32.add
                            (get_local $4)
                            (i32.shl
                              (get_local $5)
                              (i32.const 2)
                            )
                          )
                          (i32.const 10)
                        )
                        (set_local $5
                          (i32.wrap/i64
                            (i64.load
                              (i32.add
                                (get_local $3)
                                (i32.shl
                                  (i32.add
                                    (i32.load8_s
                                      (get_local $8)
                                    )
                                    (i32.const -48)
                                  )
                                  (i32.const 3)
                                )
                              )
                            )
                          )
                        )
                        (set_local $11
                          (i32.add
                            (get_local $11)
                            (i32.const 4)
                          )
                        )
                        (br $label$break$L45)
                      )
                    )
                  )
                  (if
                    (get_local $17)
                    (block
                      (set_local $15
                        (i32.const -1)
                      )
                      (br $label$break$L1)
                    )
                  )
                  (set_local $11
                    (if i32
                      (get_local $34)
                      (block i32
                        (set_local $5
                          (i32.load
                            (tee_local $11
                              (i32.and
                                (i32.add
                                  (i32.load
                                    (get_local $2)
                                  )
                                  (i32.const 3)
                                )
                                (i32.const -4)
                              )
                            )
                          )
                        )
                        (i32.store
                          (get_local $2)
                          (i32.add
                            (get_local $11)
                            (i32.const 4)
                          )
                        )
                        (get_local $8)
                      )
                      (block i32
                        (set_local $5
                          (i32.const 0)
                        )
                        (get_local $8)
                      )
                    )
                  )
                )
                (set_local $5
                  (i32.const -1)
                )
              )
            )
            (set_local $7
              (i32.const 0)
            )
            (set_local $8
              (get_local $11)
            )
            (loop $while-in13
              (if
                (i32.gt_u
                  (tee_local $6
                    (i32.add
                      (i32.load8_s
                        (get_local $8)
                      )
                      (i32.const -65)
                    )
                  )
                  (i32.const 57)
                )
                (block
                  (set_local $15
                    (i32.const -1)
                  )
                  (br $label$break$L1)
                )
              )
              (set_local $11
                (i32.add
                  (get_local $8)
                  (i32.const 1)
                )
              )
              (if
                (i32.lt_u
                  (i32.add
                    (tee_local $6
                      (i32.and
                        (tee_local $13
                          (i32.load8_s
                            (i32.add
                              (i32.add
                                (i32.mul
                                  (get_local $7)
                                  (i32.const 58)
                                )
                                (i32.const 2704)
                              )
                              (get_local $6)
                            )
                          )
                        )
                        (i32.const 255)
                      )
                    )
                    (i32.const -1)
                  )
                  (i32.const 8)
                )
                (block
                  (set_local $7
                    (get_local $6)
                  )
                  (set_local $8
                    (get_local $11)
                  )
                  (br $while-in13)
                )
              )
            )
            (if
              (i32.eqz
                (i32.shr_s
                  (i32.shl
                    (get_local $13)
                    (i32.const 24)
                  )
                  (i32.const 24)
                )
              )
              (block
                (set_local $15
                  (i32.const -1)
                )
                (br $label$break$L1)
              )
            )
            (set_local $21
              (i32.gt_s
                (get_local $9)
                (i32.const -1)
              )
            )
            (block $__rjto$2
              (block $__rjti$2
                (if
                  (i32.eq
                    (i32.shr_s
                      (i32.shl
                        (get_local $13)
                        (i32.const 24)
                      )
                      (i32.const 24)
                    )
                    (i32.const 19)
                  )
                  (if
                    (get_local $21)
                    (block
                      (set_local $15
                        (i32.const -1)
                      )
                      (br $label$break$L1)
                    )
                    (br $__rjti$2)
                  )
                  (block
                    (if
                      (get_local $21)
                      (block
                        (i32.store
                          (i32.add
                            (get_local $4)
                            (i32.shl
                              (get_local $9)
                              (i32.const 2)
                            )
                          )
                          (get_local $6)
                        )
                        (i64.store
                          (get_local $16)
                          (i64.load
                            (i32.add
                              (get_local $3)
                              (i32.shl
                                (get_local $9)
                                (i32.const 3)
                              )
                            )
                          )
                        )
                        (br $__rjti$2)
                      )
                    )
                    (if
                      (i32.eqz
                        (get_local $34)
                      )
                      (block
                        (set_local $15
                          (i32.const 0)
                        )
                        (br $label$break$L1)
                      )
                    )
                    (call $_pop_arg
                      (get_local $16)
                      (get_local $6)
                      (get_local $2)
                    )
                  )
                )
                (br $__rjto$2)
              )
              (if
                (i32.eqz
                  (get_local $34)
                )
                (block
                  (set_local $10
                    (i32.const 0)
                  )
                  (set_local $1
                    (get_local $11)
                  )
                  (br $label$continue$L1)
                )
              )
            )
            (set_local $9
              (i32.and
                (tee_local $8
                  (i32.load8_s
                    (get_local $8)
                  )
                )
                (i32.const -33)
              )
            )
            (if
              (i32.eqz
                (i32.and
                  (i32.ne
                    (get_local $7)
                    (i32.const 0)
                  )
                  (i32.eq
                    (i32.and
                      (get_local $8)
                      (i32.const 15)
                    )
                    (i32.const 3)
                  )
                )
              )
              (set_local $9
                (get_local $8)
              )
            )
            (set_local $6
              (i32.and
                (get_local $12)
                (i32.const -65537)
              )
            )
            (if
              (i32.and
                (get_local $12)
                (i32.const 8192)
              )
              (set_local $12
                (get_local $6)
              )
            )
            (block $__rjto$8
              (block $__rjti$8
                (block $__rjti$7
                  (block $__rjti$6
                    (block $__rjti$5
                      (block $__rjti$4
                        (block $__rjti$3
                          (block $switch-default120
                            (block $switch-case42
                              (block $switch-case41
                                (block $switch-case40
                                  (block $switch-case39
                                    (block $switch-case38
                                      (block $switch-case37
                                        (block $switch-case36
                                          (block $switch-case34
                                            (block $switch-case33
                                              (block $switch-case29
                                                (block $switch-case28
                                                  (block $switch-case27
                                                    (br_table $switch-case42 $switch-default120 $switch-case40 $switch-default120 $switch-case42 $switch-case42 $switch-case42 $switch-default120 $switch-default120 $switch-default120 $switch-default120 $switch-default120 $switch-default120 $switch-default120 $switch-default120 $switch-default120 $switch-default120 $switch-default120 $switch-case41 $switch-default120 $switch-default120 $switch-default120 $switch-default120 $switch-case29 $switch-default120 $switch-default120 $switch-default120 $switch-default120 $switch-default120 $switch-default120 $switch-default120 $switch-default120 $switch-case42 $switch-default120 $switch-case37 $switch-case34 $switch-case42 $switch-case42 $switch-case42 $switch-default120 $switch-case34 $switch-default120 $switch-default120 $switch-default120 $switch-case38 $switch-case27 $switch-case33 $switch-case28 $switch-default120 $switch-default120 $switch-case39 $switch-default120 $switch-case36 $switch-default120 $switch-default120 $switch-case29 $switch-default120
                                                      (i32.sub
                                                        (get_local $9)
                                                        (i32.const 65)
                                                      )
                                                    )
                                                  )
                                                  (block $switch-default26
                                                    (block $switch-case25
                                                      (block $switch-case24
                                                        (block $switch-case23
                                                          (block $switch-case22
                                                            (block $switch-case21
                                                              (block $switch-case20
                                                                (block $switch-case19
                                                                  (br_table $switch-case19 $switch-case20 $switch-case21 $switch-case22 $switch-case23 $switch-default26 $switch-case24 $switch-case25 $switch-default26
                                                                    (i32.sub
                                                                      (i32.shr_s
                                                                        (i32.shl
                                                                          (i32.and
                                                                            (get_local $7)
                                                                            (i32.const 255)
                                                                          )
                                                                          (i32.const 24)
                                                                        )
                                                                        (i32.const 24)
                                                                      )
                                                                      (i32.const 0)
                                                                    )
                                                                  )
                                                                )
                                                                (i32.store
                                                                  (i32.load
                                                                    (get_local $16)
                                                                  )
                                                                  (get_local $15)
                                                                )
                                                                (set_local $10
                                                                  (i32.const 0)
                                                                )
                                                                (set_local $1
                                                                  (get_local $11)
                                                                )
                                                                (br $label$continue$L1)
                                                              )
                                                              (i32.store
                                                                (i32.load
                                                                  (get_local $16)
                                                                )
                                                                (get_local $15)
                                                              )
                                                              (set_local $10
                                                                (i32.const 0)
                                                              )
                                                              (set_local $1
                                                                (get_local $11)
                                                              )
                                                              (br $label$continue$L1)
                                                            )
                                                            (i64.store
                                                              (i32.load
                                                                (get_local $16)
                                                              )
                                                              (i64.extend_s/i32
                                                                (get_local $15)
                                                              )
                                                            )
                                                            (set_local $10
                                                              (i32.const 0)
                                                            )
                                                            (set_local $1
                                                              (get_local $11)
                                                            )
                                                            (br $label$continue$L1)
                                                          )
                                                          (i32.store16
                                                            (i32.load
                                                              (get_local $16)
                                                            )
                                                            (get_local $15)
                                                          )
                                                          (set_local $10
                                                            (i32.const 0)
                                                          )
                                                          (set_local $1
                                                            (get_local $11)
                                                          )
                                                          (br $label$continue$L1)
                                                        )
                                                        (i32.store8
                                                          (i32.load
                                                            (get_local $16)
                                                          )
                                                          (get_local $15)
                                                        )
                                                        (set_local $10
                                                          (i32.const 0)
                                                        )
                                                        (set_local $1
                                                          (get_local $11)
                                                        )
                                                        (br $label$continue$L1)
                                                      )
                                                      (i32.store
                                                        (i32.load
                                                          (get_local $16)
                                                        )
                                                        (get_local $15)
                                                      )
                                                      (set_local $10
                                                        (i32.const 0)
                                                      )
                                                      (set_local $1
                                                        (get_local $11)
                                                      )
                                                      (br $label$continue$L1)
                                                    )
                                                    (i64.store
                                                      (i32.load
                                                        (get_local $16)
                                                      )
                                                      (i64.extend_s/i32
                                                        (get_local $15)
                                                      )
                                                    )
                                                    (set_local $10
                                                      (i32.const 0)
                                                    )
                                                    (set_local $1
                                                      (get_local $11)
                                                    )
                                                    (br $label$continue$L1)
                                                  )
                                                  (set_local $10
                                                    (i32.const 0)
                                                  )
                                                  (set_local $1
                                                    (get_local $11)
                                                  )
                                                  (br $label$continue$L1)
                                                )
                                                (set_local $9
                                                  (i32.const 120)
                                                )
                                                (if
                                                  (i32.le_u
                                                    (get_local $5)
                                                    (i32.const 8)
                                                  )
                                                  (set_local $5
                                                    (i32.const 8)
                                                  )
                                                )
                                                (set_local $12
                                                  (i32.or
                                                    (get_local $12)
                                                    (i32.const 8)
                                                  )
                                                )
                                                (br $__rjti$3)
                                              )
                                              (br $__rjti$3)
                                            )
                                            (if
                                              (i64.eq
                                                (tee_local $20
                                                  (i64.load
                                                    (get_local $16)
                                                  )
                                                )
                                                (i64.const 0)
                                              )
                                              (set_local $6
                                                (get_local $25)
                                              )
                                              (block
                                                (set_local $1
                                                  (get_local $25)
                                                )
                                                (loop $while-in32
                                                  (i64.store8
                                                    (tee_local $1
                                                      (i32.add
                                                        (get_local $1)
                                                        (i32.const -1)
                                                      )
                                                    )
                                                    (i64.or
                                                      (i64.and
                                                        (get_local $20)
                                                        (i64.const 7)
                                                      )
                                                      (i64.const 48)
                                                    )
                                                  )
                                                  (br_if $while-in32
                                                    (i64.ne
                                                      (tee_local $20
                                                        (i64.shr_u
                                                          (get_local $20)
                                                          (i64.const 3)
                                                        )
                                                      )
                                                      (i64.const 0)
                                                    )
                                                  )
                                                  (set_local $6
                                                    (get_local $1)
                                                  )
                                                )
                                              )
                                            )
                                            (if
                                              (i32.and
                                                (get_local $12)
                                                (i32.const 8)
                                              )
                                              (block
                                                (set_local $9
                                                  (i32.add
                                                    (tee_local $1
                                                      (i32.sub
                                                        (get_local $42)
                                                        (get_local $6)
                                                      )
                                                    )
                                                    (i32.const 1)
                                                  )
                                                )
                                                (set_local $7
                                                  (i32.const 0)
                                                )
                                                (set_local $8
                                                  (i32.const 3184)
                                                )
                                                (if
                                                  (i32.le_s
                                                    (get_local $5)
                                                    (get_local $1)
                                                  )
                                                  (set_local $5
                                                    (get_local $9)
                                                  )
                                                )
                                                (br $__rjti$8)
                                              )
                                              (block
                                                (set_local $7
                                                  (i32.const 0)
                                                )
                                                (set_local $8
                                                  (i32.const 3184)
                                                )
                                                (br $__rjti$8)
                                              )
                                            )
                                          )
                                          (if
                                            (i64.lt_s
                                              (tee_local $20
                                                (i64.load
                                                  (get_local $16)
                                                )
                                              )
                                              (i64.const 0)
                                            )
                                            (block
                                              (i64.store
                                                (get_local $16)
                                                (tee_local $20
                                                  (i64.sub
                                                    (i64.const 0)
                                                    (get_local $20)
                                                  )
                                                )
                                              )
                                              (set_local $7
                                                (i32.const 1)
                                              )
                                              (set_local $8
                                                (i32.const 3184)
                                              )
                                              (br $__rjti$4)
                                            )
                                          )
                                          (if
                                            (i32.and
                                              (get_local $12)
                                              (i32.const 2048)
                                            )
                                            (block
                                              (set_local $7
                                                (i32.const 1)
                                              )
                                              (set_local $8
                                                (i32.const 3185)
                                              )
                                              (br $__rjti$4)
                                            )
                                            (block
                                              (set_local $7
                                                (tee_local $1
                                                  (i32.and
                                                    (get_local $12)
                                                    (i32.const 1)
                                                  )
                                                )
                                              )
                                              (set_local $8
                                                (if i32
                                                  (get_local $1)
                                                  (i32.const 3186)
                                                  (i32.const 3184)
                                                )
                                              )
                                              (br $__rjti$4)
                                            )
                                          )
                                        )
                                        (set_local $7
                                          (i32.const 0)
                                        )
                                        (set_local $8
                                          (i32.const 3184)
                                        )
                                        (set_local $20
                                          (i64.load
                                            (get_local $16)
                                          )
                                        )
                                        (br $__rjti$4)
                                      )
                                      (i64.store8
                                        (get_local $43)
                                        (i64.load
                                          (get_local $16)
                                        )
                                      )
                                      (set_local $1
                                        (get_local $43)
                                      )
                                      (set_local $7
                                        (i32.const 0)
                                      )
                                      (set_local $9
                                        (i32.const 3184)
                                      )
                                      (set_local $8
                                        (get_local $25)
                                      )
                                      (set_local $5
                                        (i32.const 1)
                                      )
                                      (set_local $12
                                        (get_local $6)
                                      )
                                      (br $__rjto$8)
                                    )
                                    (set_local $1
                                      (call $_strerror
                                        (i32.load
                                          (call $___errno_location)
                                        )
                                      )
                                    )
                                    (br $__rjti$5)
                                  )
                                  (if
                                    (i32.eqz
                                      (tee_local $1
                                        (i32.load
                                          (get_local $16)
                                        )
                                      )
                                    )
                                    (set_local $1
                                      (i32.const 3194)
                                    )
                                  )
                                  (br $__rjti$5)
                                )
                                (i64.store32
                                  (get_local $39)
                                  (i64.load
                                    (get_local $16)
                                  )
                                )
                                (i32.store
                                  (get_local $46)
                                  (i32.const 0)
                                )
                                (i32.store
                                  (get_local $16)
                                  (get_local $39)
                                )
                                (set_local $7
                                  (i32.const -1)
                                )
                                (set_local $5
                                  (get_local $39)
                                )
                                (br $__rjti$6)
                              )
                              (set_local $1
                                (i32.load
                                  (get_local $16)
                                )
                              )
                              (if
                                (get_local $5)
                                (block
                                  (set_local $7
                                    (get_local $5)
                                  )
                                  (set_local $5
                                    (get_local $1)
                                  )
                                  (br $__rjti$6)
                                )
                                (block
                                  (call $_pad
                                    (get_local $0)
                                    (i32.const 32)
                                    (get_local $10)
                                    (i32.const 0)
                                    (get_local $12)
                                  )
                                  (set_local $1
                                    (i32.const 0)
                                  )
                                  (br $__rjti$7)
                                )
                              )
                            )
                            (set_local $14
                              (f64.load
                                (get_local $16)
                              )
                            )
                            (i32.store
                              (get_local $23)
                              (i32.const 0)
                            )
                            (set_local $29
                              (if i32
                                (i64.lt_s
                                  (i64.reinterpret/f64
                                    (get_local $14)
                                  )
                                  (i64.const 0)
                                )
                                (block i32
                                  (set_local $14
                                    (f64.neg
                                      (get_local $14)
                                    )
                                  )
                                  (set_local $21
                                    (i32.const 1)
                                  )
                                  (i32.const 3201)
                                )
                                (block i32
                                  (set_local $1
                                    (i32.and
                                      (get_local $12)
                                      (i32.const 1)
                                    )
                                  )
                                  (if i32
                                    (i32.and
                                      (get_local $12)
                                      (i32.const 2048)
                                    )
                                    (block i32
                                      (set_local $21
                                        (i32.const 1)
                                      )
                                      (i32.const 3204)
                                    )
                                    (block i32
                                      (set_local $21
                                        (get_local $1)
                                      )
                                      (if i32
                                        (get_local $1)
                                        (i32.const 3207)
                                        (i32.const 3202)
                                      )
                                    )
                                  )
                                )
                              )
                            )
                            (block $do-once49
                              (if
                                (i64.lt_u
                                  (i64.and
                                    (i64.reinterpret/f64
                                      (get_local $14)
                                    )
                                    (i64.const 9218868437227405312)
                                  )
                                  (i64.const 9218868437227405312)
                                )
                                (block
                                  (if
                                    (tee_local $1
                                      (f64.ne
                                        (tee_local $14
                                          (f64.mul
                                            (call $_frexpl
                                              (get_local $14)
                                              (get_local $23)
                                            )
                                            (f64.const 2)
                                          )
                                        )
                                        (f64.const 0)
                                      )
                                    )
                                    (i32.store
                                      (get_local $23)
                                      (i32.add
                                        (i32.load
                                          (get_local $23)
                                        )
                                        (i32.const -1)
                                      )
                                    )
                                  )
                                  (if
                                    (i32.eq
                                      (tee_local $18
                                        (i32.or
                                          (get_local $9)
                                          (i32.const 32)
                                        )
                                      )
                                      (i32.const 97)
                                    )
                                    (block
                                      (set_local $1
                                        (i32.add
                                          (get_local $29)
                                          (i32.const 9)
                                        )
                                      )
                                      (if
                                        (tee_local $7
                                          (i32.and
                                            (get_local $9)
                                            (i32.const 32)
                                          )
                                        )
                                        (set_local $29
                                          (get_local $1)
                                        )
                                      )
                                      (if
                                        (i32.eqz
                                          (i32.or
                                            (i32.gt_u
                                              (get_local $5)
                                              (i32.const 11)
                                            )
                                            (i32.eqz
                                              (tee_local $1
                                                (i32.sub
                                                  (i32.const 12)
                                                  (get_local $5)
                                                )
                                              )
                                            )
                                          )
                                        )
                                        (block
                                          (set_local $26
                                            (f64.const 8)
                                          )
                                          (loop $while-in54
                                            (set_local $26
                                              (f64.mul
                                                (get_local $26)
                                                (f64.const 16)
                                              )
                                            )
                                            (br_if $while-in54
                                              (tee_local $1
                                                (i32.add
                                                  (get_local $1)
                                                  (i32.const -1)
                                                )
                                              )
                                            )
                                          )
                                          (set_local $14
                                            (if f64
                                              (i32.eq
                                                (i32.load8_s
                                                  (get_local $29)
                                                )
                                                (i32.const 45)
                                              )
                                              (f64.neg
                                                (f64.add
                                                  (get_local $26)
                                                  (f64.sub
                                                    (f64.neg
                                                      (get_local $14)
                                                    )
                                                    (get_local $26)
                                                  )
                                                )
                                              )
                                              (f64.sub
                                                (f64.add
                                                  (get_local $14)
                                                  (get_local $26)
                                                )
                                                (get_local $26)
                                              )
                                            )
                                          )
                                        )
                                      )
                                      (set_local $1
                                        (i32.sub
                                          (i32.const 0)
                                          (tee_local $8
                                            (i32.load
                                              (get_local $23)
                                            )
                                          )
                                        )
                                      )
                                      (if
                                        (i32.eq
                                          (tee_local $1
                                            (call $_fmt_u
                                              (i64.extend_s/i32
                                                (if i32
                                                  (i32.lt_s
                                                    (get_local $8)
                                                    (i32.const 0)
                                                  )
                                                  (get_local $1)
                                                  (get_local $8)
                                                )
                                              )
                                              (get_local $36)
                                            )
                                          )
                                          (get_local $36)
                                        )
                                        (block
                                          (i32.store8
                                            (get_local $44)
                                            (i32.const 48)
                                          )
                                          (set_local $1
                                            (get_local $44)
                                          )
                                        )
                                      )
                                      (set_local $13
                                        (i32.or
                                          (get_local $21)
                                          (i32.const 2)
                                        )
                                      )
                                      (i32.store8
                                        (i32.add
                                          (get_local $1)
                                          (i32.const -1)
                                        )
                                        (i32.add
                                          (i32.and
                                            (i32.shr_s
                                              (get_local $8)
                                              (i32.const 31)
                                            )
                                            (i32.const 2)
                                          )
                                          (i32.const 43)
                                        )
                                      )
                                      (i32.store8
                                        (tee_local $6
                                          (i32.add
                                            (get_local $1)
                                            (i32.const -2)
                                          )
                                        )
                                        (i32.add
                                          (get_local $9)
                                          (i32.const 15)
                                        )
                                      )
                                      (set_local $9
                                        (i32.lt_s
                                          (get_local $5)
                                          (i32.const 1)
                                        )
                                      )
                                      (set_local $21
                                        (i32.eqz
                                          (i32.and
                                            (get_local $12)
                                            (i32.const 8)
                                          )
                                        )
                                      )
                                      (set_local $1
                                        (get_local $22)
                                      )
                                      (loop $while-in56
                                        (i32.store8
                                          (get_local $1)
                                          (i32.or
                                            (i32.load8_u
                                              (i32.add
                                                (tee_local $8
                                                  (call $f64-to-int
                                                    (get_local $14)
                                                  )
                                                )
                                                (i32.const 3168)
                                              )
                                            )
                                            (get_local $7)
                                          )
                                        )
                                        (set_local $14
                                          (f64.mul
                                            (f64.sub
                                              (get_local $14)
                                              (f64.convert_s/i32
                                                (get_local $8)
                                              )
                                            )
                                            (f64.const 16)
                                          )
                                        )
                                        (set_local $1
                                          (block $do-once57 i32
                                            (if i32
                                              (i32.eq
                                                (i32.sub
                                                  (tee_local $8
                                                    (i32.add
                                                      (get_local $1)
                                                      (i32.const 1)
                                                    )
                                                  )
                                                  (get_local $30)
                                                )
                                                (i32.const 1)
                                              )
                                              (block i32
                                                (drop
                                                  (br_if $do-once57
                                                    (get_local $8)
                                                    (i32.and
                                                      (get_local $21)
                                                      (i32.and
                                                        (get_local $9)
                                                        (f64.eq
                                                          (get_local $14)
                                                          (f64.const 0)
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                (i32.store8
                                                  (get_local $8)
                                                  (i32.const 46)
                                                )
                                                (i32.add
                                                  (get_local $1)
                                                  (i32.const 2)
                                                )
                                              )
                                              (get_local $8)
                                            )
                                          )
                                        )
                                        (br_if $while-in56
                                          (f64.ne
                                            (get_local $14)
                                            (f64.const 0)
                                          )
                                        )
                                      )
                                      (set_local $7
                                        (i32.sub
                                          (i32.add
                                            (get_local $50)
                                            (get_local $5)
                                          )
                                          (tee_local $8
                                            (get_local $6)
                                          )
                                        )
                                      )
                                      (set_local $9
                                        (i32.add
                                          (i32.sub
                                            (get_local $48)
                                            (get_local $8)
                                          )
                                          (get_local $1)
                                        )
                                      )
                                      (call $_pad
                                        (get_local $0)
                                        (i32.const 32)
                                        (get_local $10)
                                        (tee_local $5
                                          (i32.add
                                            (if i32
                                              (i32.and
                                                (i32.ne
                                                  (get_local $5)
                                                  (i32.const 0)
                                                )
                                                (i32.lt_s
                                                  (i32.add
                                                    (get_local $49)
                                                    (get_local $1)
                                                  )
                                                  (get_local $5)
                                                )
                                              )
                                              (get_local $7)
                                              (tee_local $7
                                                (get_local $9)
                                              )
                                            )
                                            (get_local $13)
                                          )
                                        )
                                        (get_local $12)
                                      )
                                      (if
                                        (i32.eqz
                                          (i32.and
                                            (i32.load
                                              (get_local $0)
                                            )
                                            (i32.const 32)
                                          )
                                        )
                                        (drop
                                          (call $___fwritex
                                            (get_local $29)
                                            (get_local $13)
                                            (get_local $0)
                                          )
                                        )
                                      )
                                      (call $_pad
                                        (get_local $0)
                                        (i32.const 48)
                                        (get_local $10)
                                        (get_local $5)
                                        (i32.xor
                                          (get_local $12)
                                          (i32.const 65536)
                                        )
                                      )
                                      (set_local $1
                                        (i32.sub
                                          (get_local $1)
                                          (get_local $30)
                                        )
                                      )
                                      (if
                                        (i32.eqz
                                          (i32.and
                                            (i32.load
                                              (get_local $0)
                                            )
                                            (i32.const 32)
                                          )
                                        )
                                        (drop
                                          (call $___fwritex
                                            (get_local $22)
                                            (get_local $1)
                                            (get_local $0)
                                          )
                                        )
                                      )
                                      (call $_pad
                                        (get_local $0)
                                        (i32.const 48)
                                        (i32.sub
                                          (get_local $7)
                                          (i32.add
                                            (get_local $1)
                                            (tee_local $1
                                              (i32.sub
                                                (get_local $31)
                                                (get_local $8)
                                              )
                                            )
                                          )
                                        )
                                        (i32.const 0)
                                        (i32.const 0)
                                      )
                                      (if
                                        (i32.eqz
                                          (i32.and
                                            (i32.load
                                              (get_local $0)
                                            )
                                            (i32.const 32)
                                          )
                                        )
                                        (drop
                                          (call $___fwritex
                                            (get_local $6)
                                            (get_local $1)
                                            (get_local $0)
                                          )
                                        )
                                      )
                                      (call $_pad
                                        (get_local $0)
                                        (i32.const 32)
                                        (get_local $10)
                                        (get_local $5)
                                        (i32.xor
                                          (get_local $12)
                                          (i32.const 8192)
                                        )
                                      )
                                      (if
                                        (i32.ge_s
                                          (get_local $5)
                                          (get_local $10)
                                        )
                                        (set_local $10
                                          (get_local $5)
                                        )
                                      )
                                      (br $do-once49)
                                    )
                                  )
                                  (if
                                    (get_local $1)
                                    (block
                                      (i32.store
                                        (get_local $23)
                                        (tee_local $7
                                          (i32.add
                                            (i32.load
                                              (get_local $23)
                                            )
                                            (i32.const -28)
                                          )
                                        )
                                      )
                                      (set_local $14
                                        (f64.mul
                                          (get_local $14)
                                          (f64.const 268435456)
                                        )
                                      )
                                    )
                                    (set_local $7
                                      (i32.load
                                        (get_local $23)
                                      )
                                    )
                                  )
                                  (set_local $6
                                    (tee_local $8
                                      (if i32
                                        (i32.lt_s
                                          (get_local $7)
                                          (i32.const 0)
                                        )
                                        (get_local $51)
                                        (get_local $52)
                                      )
                                    )
                                  )
                                  (loop $while-in60
                                    (i32.store
                                      (get_local $6)
                                      (tee_local $1
                                        (call $f64-to-int
                                          (get_local $14)
                                        )
                                      )
                                    )
                                    (set_local $6
                                      (i32.add
                                        (get_local $6)
                                        (i32.const 4)
                                      )
                                    )
                                    (br_if $while-in60
                                      (f64.ne
                                        (tee_local $14
                                          (f64.mul
                                            (f64.sub
                                              (get_local $14)
                                              (f64.convert_u/i32
                                                (get_local $1)
                                              )
                                            )
                                            (f64.const 1e9)
                                          )
                                        )
                                        (f64.const 0)
                                      )
                                    )
                                  )
                                  (if
                                    (i32.gt_s
                                      (get_local $7)
                                      (i32.const 0)
                                    )
                                    (block
                                      (set_local $1
                                        (get_local $8)
                                      )
                                      (loop $while-in62
                                        (set_local $19
                                          (if i32
                                            (i32.gt_s
                                              (get_local $7)
                                              (i32.const 29)
                                            )
                                            (i32.const 29)
                                            (get_local $7)
                                          )
                                        )
                                        (block $do-once63
                                          (if
                                            (i32.ge_u
                                              (tee_local $7
                                                (i32.add
                                                  (get_local $6)
                                                  (i32.const -4)
                                                )
                                              )
                                              (get_local $1)
                                            )
                                            (block
                                              (set_local $20
                                                (i64.extend_u/i32
                                                  (get_local $19)
                                                )
                                              )
                                              (set_local $13
                                                (i32.const 0)
                                              )
                                              (loop $while-in66
                                                (i64.store32
                                                  (get_local $7)
                                                  (call $i64u-rem
                                                    (tee_local $53
                                                      (i64.add
                                                        (i64.shl
                                                          (i64.extend_u/i32
                                                            (i32.load
                                                              (get_local $7)
                                                            )
                                                          )
                                                          (get_local $20)
                                                        )
                                                        (i64.extend_u/i32
                                                          (get_local $13)
                                                        )
                                                      )
                                                    )
                                                    (i64.const 1000000000)
                                                  )
                                                )
                                                (set_local $13
                                                  (i32.wrap/i64
                                                    (call $i64u-div
                                                      (get_local $53)
                                                      (i64.const 1000000000)
                                                    )
                                                  )
                                                )
                                                (br_if $while-in66
                                                  (i32.ge_u
                                                    (tee_local $7
                                                      (i32.add
                                                        (get_local $7)
                                                        (i32.const -4)
                                                      )
                                                    )
                                                    (get_local $1)
                                                  )
                                                )
                                              )
                                              (br_if $do-once63
                                                (i32.eqz
                                                  (get_local $13)
                                                )
                                              )
                                              (i32.store
                                                (tee_local $1
                                                  (i32.add
                                                    (get_local $1)
                                                    (i32.const -4)
                                                  )
                                                )
                                                (get_local $13)
                                              )
                                            )
                                          )
                                        )
                                        (loop $while-in68
                                          (if
                                            (i32.gt_u
                                              (get_local $6)
                                              (get_local $1)
                                            )
                                            (if
                                              (i32.eqz
                                                (i32.load
                                                  (tee_local $7
                                                    (i32.add
                                                      (get_local $6)
                                                      (i32.const -4)
                                                    )
                                                  )
                                                )
                                              )
                                              (block
                                                (set_local $6
                                                  (get_local $7)
                                                )
                                                (br $while-in68)
                                              )
                                            )
                                          )
                                        )
                                        (i32.store
                                          (get_local $23)
                                          (tee_local $7
                                            (i32.sub
                                              (i32.load
                                                (get_local $23)
                                              )
                                              (get_local $19)
                                            )
                                          )
                                        )
                                        (br_if $while-in62
                                          (i32.gt_s
                                            (get_local $7)
                                            (i32.const 0)
                                          )
                                        )
                                      )
                                    )
                                    (set_local $1
                                      (get_local $8)
                                    )
                                  )
                                  (set_local $19
                                    (if i32
                                      (i32.lt_s
                                        (get_local $5)
                                        (i32.const 0)
                                      )
                                      (i32.const 6)
                                      (get_local $5)
                                    )
                                  )
                                  (if
                                    (i32.lt_s
                                      (get_local $7)
                                      (i32.const 0)
                                    )
                                    (block
                                      (set_local $24
                                        (i32.add
                                          (call $i32s-div
                                            (i32.add
                                              (get_local $19)
                                              (i32.const 25)
                                            )
                                            (i32.const 9)
                                          )
                                          (i32.const 1)
                                        )
                                      )
                                      (set_local $28
                                        (i32.eq
                                          (get_local $18)
                                          (i32.const 102)
                                        )
                                      )
                                      (set_local $5
                                        (get_local $6)
                                      )
                                      (loop $while-in70
                                        (if
                                          (i32.gt_s
                                            (tee_local $13
                                              (i32.sub
                                                (i32.const 0)
                                                (get_local $7)
                                              )
                                            )
                                            (i32.const 9)
                                          )
                                          (set_local $13
                                            (i32.const 9)
                                          )
                                        )
                                        (block $do-once71
                                          (if
                                            (i32.lt_u
                                              (get_local $1)
                                              (get_local $5)
                                            )
                                            (block
                                              (set_local $32
                                                (i32.add
                                                  (i32.shl
                                                    (i32.const 1)
                                                    (get_local $13)
                                                  )
                                                  (i32.const -1)
                                                )
                                              )
                                              (set_local $33
                                                (i32.shr_u
                                                  (i32.const 1000000000)
                                                  (get_local $13)
                                                )
                                              )
                                              (set_local $7
                                                (i32.const 0)
                                              )
                                              (set_local $6
                                                (get_local $1)
                                              )
                                              (loop $while-in74
                                                (i32.store
                                                  (get_local $6)
                                                  (i32.add
                                                    (i32.shr_u
                                                      (tee_local $40
                                                        (i32.load
                                                          (get_local $6)
                                                        )
                                                      )
                                                      (get_local $13)
                                                    )
                                                    (get_local $7)
                                                  )
                                                )
                                                (set_local $7
                                                  (i32.mul
                                                    (i32.and
                                                      (get_local $40)
                                                      (get_local $32)
                                                    )
                                                    (get_local $33)
                                                  )
                                                )
                                                (br_if $while-in74
                                                  (i32.lt_u
                                                    (tee_local $6
                                                      (i32.add
                                                        (get_local $6)
                                                        (i32.const 4)
                                                      )
                                                    )
                                                    (get_local $5)
                                                  )
                                                )
                                              )
                                              (set_local $6
                                                (i32.add
                                                  (get_local $1)
                                                  (i32.const 4)
                                                )
                                              )
                                              (if
                                                (i32.eqz
                                                  (i32.load
                                                    (get_local $1)
                                                  )
                                                )
                                                (set_local $1
                                                  (get_local $6)
                                                )
                                              )
                                              (br_if $do-once71
                                                (i32.eqz
                                                  (get_local $7)
                                                )
                                              )
                                              (i32.store
                                                (get_local $5)
                                                (get_local $7)
                                              )
                                              (set_local $5
                                                (i32.add
                                                  (get_local $5)
                                                  (i32.const 4)
                                                )
                                              )
                                            )
                                            (block
                                              (set_local $6
                                                (i32.add
                                                  (get_local $1)
                                                  (i32.const 4)
                                                )
                                              )
                                              (if
                                                (i32.eqz
                                                  (i32.load
                                                    (get_local $1)
                                                  )
                                                )
                                                (set_local $1
                                                  (get_local $6)
                                                )
                                              )
                                            )
                                          )
                                        )
                                        (set_local $7
                                          (i32.add
                                            (tee_local $6
                                              (if i32
                                                (get_local $28)
                                                (get_local $8)
                                                (get_local $1)
                                              )
                                            )
                                            (i32.shl
                                              (get_local $24)
                                              (i32.const 2)
                                            )
                                          )
                                        )
                                        (if
                                          (i32.gt_s
                                            (i32.shr_s
                                              (i32.sub
                                                (get_local $5)
                                                (get_local $6)
                                              )
                                              (i32.const 2)
                                            )
                                            (get_local $24)
                                          )
                                          (set_local $5
                                            (get_local $7)
                                          )
                                        )
                                        (i32.store
                                          (get_local $23)
                                          (tee_local $7
                                            (i32.add
                                              (i32.load
                                                (get_local $23)
                                              )
                                              (get_local $13)
                                            )
                                          )
                                        )
                                        (br_if $while-in70
                                          (i32.lt_s
                                            (get_local $7)
                                            (i32.const 0)
                                          )
                                        )
                                        (set_local $13
                                          (get_local $5)
                                        )
                                      )
                                    )
                                    (set_local $13
                                      (get_local $6)
                                    )
                                  )
                                  (set_local $24
                                    (get_local $8)
                                  )
                                  (block $do-once75
                                    (if
                                      (i32.lt_u
                                        (get_local $1)
                                        (get_local $13)
                                      )
                                      (block
                                        (set_local $5
                                          (i32.mul
                                            (i32.shr_s
                                              (i32.sub
                                                (get_local $24)
                                                (get_local $1)
                                              )
                                              (i32.const 2)
                                            )
                                            (i32.const 9)
                                          )
                                        )
                                        (br_if $do-once75
                                          (i32.lt_u
                                            (tee_local $7
                                              (i32.load
                                                (get_local $1)
                                              )
                                            )
                                            (i32.const 10)
                                          )
                                        )
                                        (set_local $6
                                          (i32.const 10)
                                        )
                                        (loop $while-in78
                                          (set_local $5
                                            (i32.add
                                              (get_local $5)
                                              (i32.const 1)
                                            )
                                          )
                                          (br_if $while-in78
                                            (i32.ge_u
                                              (get_local $7)
                                              (tee_local $6
                                                (i32.mul
                                                  (get_local $6)
                                                  (i32.const 10)
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                      (set_local $5
                                        (i32.const 0)
                                      )
                                    )
                                  )
                                  (set_local $28
                                    (i32.eq
                                      (get_local $18)
                                      (i32.const 103)
                                    )
                                  )
                                  (set_local $32
                                    (i32.ne
                                      (get_local $19)
                                      (i32.const 0)
                                    )
                                  )
                                  (set_local $13
                                    (if i32
                                      (i32.lt_s
                                        (tee_local $6
                                          (i32.add
                                            (i32.sub
                                              (get_local $19)
                                              (if i32
                                                (i32.ne
                                                  (get_local $18)
                                                  (i32.const 102)
                                                )
                                                (get_local $5)
                                                (i32.const 0)
                                              )
                                            )
                                            (i32.shr_s
                                              (i32.shl
                                                (i32.and
                                                  (get_local $32)
                                                  (get_local $28)
                                                )
                                                (i32.const 31)
                                              )
                                              (i32.const 31)
                                            )
                                          )
                                        )
                                        (i32.add
                                          (i32.mul
                                            (i32.shr_s
                                              (i32.sub
                                                (get_local $13)
                                                (get_local $24)
                                              )
                                              (i32.const 2)
                                            )
                                            (i32.const 9)
                                          )
                                          (i32.const -9)
                                        )
                                      )
                                      (block i32
                                        (set_local $18
                                          (call $i32s-div
                                            (tee_local $6
                                              (i32.add
                                                (get_local $6)
                                                (i32.const 9216)
                                              )
                                            )
                                            (i32.const 9)
                                          )
                                        )
                                        (if
                                          (i32.lt_s
                                            (tee_local $6
                                              (i32.add
                                                (call $i32s-rem
                                                  (get_local $6)
                                                  (i32.const 9)
                                                )
                                                (i32.const 1)
                                              )
                                            )
                                            (i32.const 9)
                                          )
                                          (block
                                            (set_local $7
                                              (i32.const 10)
                                            )
                                            (loop $while-in80
                                              (set_local $7
                                                (i32.mul
                                                  (get_local $7)
                                                  (i32.const 10)
                                                )
                                              )
                                              (br_if $while-in80
                                                (i32.ne
                                                  (tee_local $6
                                                    (i32.add
                                                      (get_local $6)
                                                      (i32.const 1)
                                                    )
                                                  )
                                                  (i32.const 9)
                                                )
                                              )
                                            )
                                          )
                                          (set_local $7
                                            (i32.const 10)
                                          )
                                        )
                                        (set_local $18
                                          (call $i32u-rem
                                            (tee_local $33
                                              (i32.load
                                                (tee_local $6
                                                  (i32.add
                                                    (i32.add
                                                      (get_local $8)
                                                      (i32.const 4)
                                                    )
                                                    (i32.shl
                                                      (i32.add
                                                        (get_local $18)
                                                        (i32.const -1024)
                                                      )
                                                      (i32.const 2)
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                            (get_local $7)
                                          )
                                        )
                                        (block $do-once81
                                          (if
                                            (i32.eqz
                                              (i32.and
                                                (tee_local $40
                                                  (i32.eq
                                                    (i32.add
                                                      (get_local $6)
                                                      (i32.const 4)
                                                    )
                                                    (get_local $13)
                                                  )
                                                )
                                                (i32.eqz
                                                  (get_local $18)
                                                )
                                              )
                                            )
                                            (block
                                              (set_local $54
                                                (call $i32u-div
                                                  (get_local $33)
                                                  (get_local $7)
                                                )
                                              )
                                              (set_local $14
                                                (if f64
                                                  (i32.lt_u
                                                    (get_local $18)
                                                    (tee_local $55
                                                      (call $i32s-div
                                                        (get_local $7)
                                                        (i32.const 2)
                                                      )
                                                    )
                                                  )
                                                  (f64.const 0.5)
                                                  (if f64
                                                    (i32.and
                                                      (get_local $40)
                                                      (i32.eq
                                                        (get_local $18)
                                                        (get_local $55)
                                                      )
                                                    )
                                                    (f64.const 1)
                                                    (f64.const 1.5)
                                                  )
                                                )
                                              )
                                              (set_local $26
                                                (if f64
                                                  (i32.and
                                                    (get_local $54)
                                                    (i32.const 1)
                                                  )
                                                  (f64.const 9007199254740994)
                                                  (f64.const 9007199254740992)
                                                )
                                              )
                                              (set_local $14
                                                (block $do-once83 f64
                                                  (if f64
                                                    (get_local $21)
                                                    (block f64
                                                      (if
                                                        (i32.ne
                                                          (i32.load8_s
                                                            (get_local $29)
                                                          )
                                                          (i32.const 45)
                                                        )
                                                        (block
                                                          (set_local $41
                                                            (get_local $14)
                                                          )
                                                          (br $do-once83
                                                            (get_local $26)
                                                          )
                                                        )
                                                      )
                                                      (set_local $41
                                                        (f64.neg
                                                          (get_local $14)
                                                        )
                                                      )
                                                      (f64.neg
                                                        (get_local $26)
                                                      )
                                                    )
                                                    (block f64
                                                      (set_local $41
                                                        (get_local $14)
                                                      )
                                                      (get_local $26)
                                                    )
                                                  )
                                                )
                                              )
                                              (i32.store
                                                (get_local $6)
                                                (tee_local $18
                                                  (i32.sub
                                                    (get_local $33)
                                                    (get_local $18)
                                                  )
                                                )
                                              )
                                              (br_if $do-once81
                                                (f64.eq
                                                  (f64.add
                                                    (get_local $14)
                                                    (get_local $41)
                                                  )
                                                  (get_local $14)
                                                )
                                              )
                                              (i32.store
                                                (get_local $6)
                                                (tee_local $5
                                                  (i32.add
                                                    (get_local $18)
                                                    (get_local $7)
                                                  )
                                                )
                                              )
                                              (if
                                                (i32.gt_u
                                                  (get_local $5)
                                                  (i32.const 999999999)
                                                )
                                                (loop $while-in86
                                                  (i32.store
                                                    (get_local $6)
                                                    (i32.const 0)
                                                  )
                                                  (if
                                                    (i32.lt_u
                                                      (tee_local $6
                                                        (i32.add
                                                          (get_local $6)
                                                          (i32.const -4)
                                                        )
                                                      )
                                                      (get_local $1)
                                                    )
                                                    (i32.store
                                                      (tee_local $1
                                                        (i32.add
                                                          (get_local $1)
                                                          (i32.const -4)
                                                        )
                                                      )
                                                      (i32.const 0)
                                                    )
                                                  )
                                                  (i32.store
                                                    (get_local $6)
                                                    (tee_local $5
                                                      (i32.add
                                                        (i32.load
                                                          (get_local $6)
                                                        )
                                                        (i32.const 1)
                                                      )
                                                    )
                                                  )
                                                  (br_if $while-in86
                                                    (i32.gt_u
                                                      (get_local $5)
                                                      (i32.const 999999999)
                                                    )
                                                  )
                                                )
                                              )
                                              (set_local $5
                                                (i32.mul
                                                  (i32.shr_s
                                                    (i32.sub
                                                      (get_local $24)
                                                      (get_local $1)
                                                    )
                                                    (i32.const 2)
                                                  )
                                                  (i32.const 9)
                                                )
                                              )
                                              (br_if $do-once81
                                                (i32.lt_u
                                                  (tee_local $18
                                                    (i32.load
                                                      (get_local $1)
                                                    )
                                                  )
                                                  (i32.const 10)
                                                )
                                              )
                                              (set_local $7
                                                (i32.const 10)
                                              )
                                              (loop $while-in88
                                                (set_local $5
                                                  (i32.add
                                                    (get_local $5)
                                                    (i32.const 1)
                                                  )
                                                )
                                                (br_if $while-in88
                                                  (i32.ge_u
                                                    (get_local $18)
                                                    (tee_local $7
                                                      (i32.mul
                                                        (get_local $7)
                                                        (i32.const 10)
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                        (set_local $7
                                          (get_local $5)
                                        )
                                        (if
                                          (i32.le_u
                                            (get_local $13)
                                            (tee_local $6
                                              (i32.add
                                                (get_local $6)
                                                (i32.const 4)
                                              )
                                            )
                                          )
                                          (set_local $6
                                            (get_local $13)
                                          )
                                        )
                                        (get_local $1)
                                      )
                                      (block i32
                                        (set_local $7
                                          (get_local $5)
                                        )
                                        (set_local $6
                                          (get_local $13)
                                        )
                                        (get_local $1)
                                      )
                                    )
                                  )
                                  (set_local $33
                                    (i32.sub
                                      (i32.const 0)
                                      (get_local $7)
                                    )
                                  )
                                  (loop $while-in90
                                    (block $while-out89
                                      (if
                                        (i32.le_u
                                          (get_local $6)
                                          (get_local $13)
                                        )
                                        (block
                                          (set_local $18
                                            (i32.const 0)
                                          )
                                          (br $while-out89)
                                        )
                                      )
                                      (if
                                        (i32.load
                                          (tee_local $1
                                            (i32.add
                                              (get_local $6)
                                              (i32.const -4)
                                            )
                                          )
                                        )
                                        (set_local $18
                                          (i32.const 1)
                                        )
                                        (block
                                          (set_local $6
                                            (get_local $1)
                                          )
                                          (br $while-in90)
                                        )
                                      )
                                    )
                                  )
                                  (block $do-once91
                                    (set_local $19
                                      (if i32
                                        (get_local $28)
                                        (block i32
                                          (set_local $1
                                            (if i32
                                              (i32.and
                                                (i32.gt_s
                                                  (tee_local $1
                                                    (i32.add
                                                      (i32.xor
                                                        (i32.and
                                                          (get_local $32)
                                                          (i32.const 1)
                                                        )
                                                        (i32.const 1)
                                                      )
                                                      (get_local $19)
                                                    )
                                                  )
                                                  (get_local $7)
                                                )
                                                (i32.gt_s
                                                  (get_local $7)
                                                  (i32.const -5)
                                                )
                                              )
                                              (block i32
                                                (set_local $5
                                                  (i32.add
                                                    (get_local $9)
                                                    (i32.const -1)
                                                  )
                                                )
                                                (i32.sub
                                                  (i32.add
                                                    (get_local $1)
                                                    (i32.const -1)
                                                  )
                                                  (get_local $7)
                                                )
                                              )
                                              (block i32
                                                (set_local $5
                                                  (i32.add
                                                    (get_local $9)
                                                    (i32.const -2)
                                                  )
                                                )
                                                (i32.add
                                                  (get_local $1)
                                                  (i32.const -1)
                                                )
                                              )
                                            )
                                          )
                                          (br_if $do-once91
                                            (tee_local $19
                                              (i32.and
                                                (get_local $12)
                                                (i32.const 8)
                                              )
                                            )
                                          )
                                          (block $do-once93
                                            (if
                                              (get_local $18)
                                              (block
                                                (if
                                                  (i32.eqz
                                                    (tee_local $28
                                                      (i32.load
                                                        (i32.add
                                                          (get_local $6)
                                                          (i32.const -4)
                                                        )
                                                      )
                                                    )
                                                  )
                                                  (block
                                                    (set_local $9
                                                      (i32.const 9)
                                                    )
                                                    (br $do-once93)
                                                  )
                                                )
                                                (if
                                                  (call $i32u-rem
                                                    (get_local $28)
                                                    (i32.const 10)
                                                  )
                                                  (block
                                                    (set_local $9
                                                      (i32.const 0)
                                                    )
                                                    (br $do-once93)
                                                  )
                                                  (block
                                                    (set_local $9
                                                      (i32.const 0)
                                                    )
                                                    (set_local $19
                                                      (i32.const 10)
                                                    )
                                                  )
                                                )
                                                (loop $while-in96
                                                  (set_local $9
                                                    (i32.add
                                                      (get_local $9)
                                                      (i32.const 1)
                                                    )
                                                  )
                                                  (br_if $while-in96
                                                    (i32.eqz
                                                      (call $i32u-rem
                                                        (get_local $28)
                                                        (tee_local $19
                                                          (i32.mul
                                                            (get_local $19)
                                                            (i32.const 10)
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                              (set_local $9
                                                (i32.const 9)
                                              )
                                            )
                                          )
                                          (set_local $19
                                            (i32.add
                                              (i32.mul
                                                (i32.shr_s
                                                  (i32.sub
                                                    (get_local $6)
                                                    (get_local $24)
                                                  )
                                                  (i32.const 2)
                                                )
                                                (i32.const 9)
                                              )
                                              (i32.const -9)
                                            )
                                          )
                                          (if i32
                                            (i32.eq
                                              (i32.or
                                                (get_local $5)
                                                (i32.const 32)
                                              )
                                              (i32.const 102)
                                            )
                                            (block i32
                                              (if
                                                (i32.ge_s
                                                  (get_local $1)
                                                  (if i32
                                                    (i32.lt_s
                                                      (tee_local $9
                                                        (i32.sub
                                                          (get_local $19)
                                                          (get_local $9)
                                                        )
                                                      )
                                                      (i32.const 0)
                                                    )
                                                    (tee_local $9
                                                      (i32.const 0)
                                                    )
                                                    (get_local $9)
                                                  )
                                                )
                                                (set_local $1
                                                  (get_local $9)
                                                )
                                              )
                                              (i32.const 0)
                                            )
                                            (block i32
                                              (if
                                                (i32.ge_s
                                                  (get_local $1)
                                                  (if i32
                                                    (i32.lt_s
                                                      (tee_local $9
                                                        (i32.sub
                                                          (i32.add
                                                            (get_local $19)
                                                            (get_local $7)
                                                          )
                                                          (get_local $9)
                                                        )
                                                      )
                                                      (i32.const 0)
                                                    )
                                                    (tee_local $9
                                                      (i32.const 0)
                                                    )
                                                    (get_local $9)
                                                  )
                                                )
                                                (set_local $1
                                                  (get_local $9)
                                                )
                                              )
                                              (i32.const 0)
                                            )
                                          )
                                        )
                                        (block i32
                                          (set_local $5
                                            (get_local $9)
                                          )
                                          (set_local $1
                                            (get_local $19)
                                          )
                                          (i32.and
                                            (get_local $12)
                                            (i32.const 8)
                                          )
                                        )
                                      )
                                    )
                                  )
                                  (if
                                    (tee_local $28
                                      (i32.eq
                                        (i32.or
                                          (get_local $5)
                                          (i32.const 32)
                                        )
                                        (i32.const 102)
                                      )
                                    )
                                    (block
                                      (set_local $9
                                        (i32.const 0)
                                      )
                                      (if
                                        (i32.le_s
                                          (get_local $7)
                                          (i32.const 0)
                                        )
                                        (set_local $7
                                          (i32.const 0)
                                        )
                                      )
                                    )
                                    (block
                                      (if
                                        (i32.lt_s
                                          (i32.sub
                                            (get_local $31)
                                            (tee_local $9
                                              (call $_fmt_u
                                                (i64.extend_s/i32
                                                  (if i32
                                                    (i32.lt_s
                                                      (get_local $7)
                                                      (i32.const 0)
                                                    )
                                                    (get_local $33)
                                                    (get_local $7)
                                                  )
                                                )
                                                (get_local $36)
                                              )
                                            )
                                          )
                                          (i32.const 2)
                                        )
                                        (loop $while-in98
                                          (i32.store8
                                            (tee_local $9
                                              (i32.add
                                                (get_local $9)
                                                (i32.const -1)
                                              )
                                            )
                                            (i32.const 48)
                                          )
                                          (br_if $while-in98
                                            (i32.lt_s
                                              (i32.sub
                                                (get_local $31)
                                                (get_local $9)
                                              )
                                              (i32.const 2)
                                            )
                                          )
                                        )
                                      )
                                      (i32.store8
                                        (i32.add
                                          (get_local $9)
                                          (i32.const -1)
                                        )
                                        (i32.add
                                          (i32.and
                                            (i32.shr_s
                                              (get_local $7)
                                              (i32.const 31)
                                            )
                                            (i32.const 2)
                                          )
                                          (i32.const 43)
                                        )
                                      )
                                      (i32.store8
                                        (tee_local $7
                                          (i32.add
                                            (get_local $9)
                                            (i32.const -2)
                                          )
                                        )
                                        (get_local $5)
                                      )
                                      (set_local $9
                                        (get_local $7)
                                      )
                                      (set_local $7
                                        (i32.sub
                                          (get_local $31)
                                          (get_local $7)
                                        )
                                      )
                                    )
                                  )
                                  (call $_pad
                                    (get_local $0)
                                    (i32.const 32)
                                    (get_local $10)
                                    (tee_local $24
                                      (i32.add
                                        (i32.add
                                          (i32.add
                                            (i32.add
                                              (get_local $21)
                                              (i32.const 1)
                                            )
                                            (get_local $1)
                                          )
                                          (i32.ne
                                            (tee_local $32
                                              (i32.or
                                                (get_local $1)
                                                (get_local $19)
                                              )
                                            )
                                            (i32.const 0)
                                          )
                                        )
                                        (get_local $7)
                                      )
                                    )
                                    (get_local $12)
                                  )
                                  (if
                                    (i32.eqz
                                      (i32.and
                                        (i32.load
                                          (get_local $0)
                                        )
                                        (i32.const 32)
                                      )
                                    )
                                    (drop
                                      (call $___fwritex
                                        (get_local $29)
                                        (get_local $21)
                                        (get_local $0)
                                      )
                                    )
                                  )
                                  (call $_pad
                                    (get_local $0)
                                    (i32.const 48)
                                    (get_local $10)
                                    (get_local $24)
                                    (i32.xor
                                      (get_local $12)
                                      (i32.const 65536)
                                    )
                                  )
                                  (block $do-once99
                                    (if
                                      (get_local $28)
                                      (block
                                        (set_local $7
                                          (tee_local $9
                                            (if i32
                                              (i32.gt_u
                                                (get_local $13)
                                                (get_local $8)
                                              )
                                              (get_local $8)
                                              (get_local $13)
                                            )
                                          )
                                        )
                                        (loop $while-in102
                                          (set_local $5
                                            (call $_fmt_u
                                              (i64.extend_u/i32
                                                (i32.load
                                                  (get_local $7)
                                                )
                                              )
                                              (get_local $35)
                                            )
                                          )
                                          (block $do-once103
                                            (if
                                              (i32.eq
                                                (get_local $7)
                                                (get_local $9)
                                              )
                                              (block
                                                (br_if $do-once103
                                                  (i32.ne
                                                    (get_local $5)
                                                    (get_local $35)
                                                  )
                                                )
                                                (i32.store8
                                                  (get_local $37)
                                                  (i32.const 48)
                                                )
                                                (set_local $5
                                                  (get_local $37)
                                                )
                                              )
                                              (block
                                                (br_if $do-once103
                                                  (i32.le_u
                                                    (get_local $5)
                                                    (get_local $22)
                                                  )
                                                )
                                                (drop
                                                  (call $_memset
                                                    (get_local $22)
                                                    (i32.const 48)
                                                    (i32.sub
                                                      (get_local $5)
                                                      (get_local $30)
                                                    )
                                                  )
                                                )
                                                (loop $while-in106
                                                  (br_if $while-in106
                                                    (i32.gt_u
                                                      (tee_local $5
                                                        (i32.add
                                                          (get_local $5)
                                                          (i32.const -1)
                                                        )
                                                      )
                                                      (get_local $22)
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                          (if
                                            (i32.eqz
                                              (i32.and
                                                (i32.load
                                                  (get_local $0)
                                                )
                                                (i32.const 32)
                                              )
                                            )
                                            (drop
                                              (call $___fwritex
                                                (get_local $5)
                                                (i32.sub
                                                  (get_local $45)
                                                  (get_local $5)
                                                )
                                                (get_local $0)
                                              )
                                            )
                                          )
                                          (if
                                            (i32.le_u
                                              (tee_local $5
                                                (i32.add
                                                  (get_local $7)
                                                  (i32.const 4)
                                                )
                                              )
                                              (get_local $8)
                                            )
                                            (block
                                              (set_local $7
                                                (get_local $5)
                                              )
                                              (br $while-in102)
                                            )
                                          )
                                        )
                                        (block $do-once107
                                          (if
                                            (get_local $32)
                                            (block
                                              (br_if $do-once107
                                                (i32.and
                                                  (i32.load
                                                    (get_local $0)
                                                  )
                                                  (i32.const 32)
                                                )
                                              )
                                              (drop
                                                (call $___fwritex
                                                  (i32.const 3236)
                                                  (i32.const 1)
                                                  (get_local $0)
                                                )
                                              )
                                            )
                                          )
                                        )
                                        (if
                                          (i32.and
                                            (i32.gt_s
                                              (get_local $1)
                                              (i32.const 0)
                                            )
                                            (i32.lt_u
                                              (get_local $5)
                                              (get_local $6)
                                            )
                                          )
                                          (loop $while-in110
                                            (if
                                              (i32.gt_u
                                                (tee_local $8
                                                  (call $_fmt_u
                                                    (i64.extend_u/i32
                                                      (i32.load
                                                        (get_local $5)
                                                      )
                                                    )
                                                    (get_local $35)
                                                  )
                                                )
                                                (get_local $22)
                                              )
                                              (block
                                                (drop
                                                  (call $_memset
                                                    (get_local $22)
                                                    (i32.const 48)
                                                    (i32.sub
                                                      (get_local $8)
                                                      (get_local $30)
                                                    )
                                                  )
                                                )
                                                (loop $while-in112
                                                  (br_if $while-in112
                                                    (i32.gt_u
                                                      (tee_local $8
                                                        (i32.add
                                                          (get_local $8)
                                                          (i32.const -1)
                                                        )
                                                      )
                                                      (get_local $22)
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                            (if
                                              (i32.eqz
                                                (i32.and
                                                  (i32.load
                                                    (get_local $0)
                                                  )
                                                  (i32.const 32)
                                                )
                                              )
                                              (drop
                                                (call $___fwritex
                                                  (get_local $8)
                                                  (if i32
                                                    (i32.gt_s
                                                      (get_local $1)
                                                      (i32.const 9)
                                                    )
                                                    (i32.const 9)
                                                    (get_local $1)
                                                  )
                                                  (get_local $0)
                                                )
                                              )
                                            )
                                            (set_local $8
                                              (i32.add
                                                (get_local $1)
                                                (i32.const -9)
                                              )
                                            )
                                            (if
                                              (i32.and
                                                (i32.gt_s
                                                  (get_local $1)
                                                  (i32.const 9)
                                                )
                                                (i32.lt_u
                                                  (tee_local $5
                                                    (i32.add
                                                      (get_local $5)
                                                      (i32.const 4)
                                                    )
                                                  )
                                                  (get_local $6)
                                                )
                                              )
                                              (block
                                                (set_local $1
                                                  (get_local $8)
                                                )
                                                (br $while-in110)
                                              )
                                              (set_local $1
                                                (get_local $8)
                                              )
                                            )
                                          )
                                        )
                                        (call $_pad
                                          (get_local $0)
                                          (i32.const 48)
                                          (i32.add
                                            (get_local $1)
                                            (i32.const 9)
                                          )
                                          (i32.const 9)
                                          (i32.const 0)
                                        )
                                      )
                                      (block
                                        (set_local $5
                                          (i32.add
                                            (get_local $13)
                                            (i32.const 4)
                                          )
                                        )
                                        (if
                                          (i32.eqz
                                            (get_local $18)
                                          )
                                          (set_local $6
                                            (get_local $5)
                                          )
                                        )
                                        (if
                                          (i32.gt_s
                                            (get_local $1)
                                            (i32.const -1)
                                          )
                                          (block
                                            (set_local $21
                                              (i32.eqz
                                                (get_local $19)
                                              )
                                            )
                                            (set_local $5
                                              (get_local $1)
                                            )
                                            (set_local $8
                                              (get_local $13)
                                            )
                                            (loop $while-in114
                                              (if
                                                (i32.eq
                                                  (tee_local $1
                                                    (call $_fmt_u
                                                      (i64.extend_u/i32
                                                        (i32.load
                                                          (get_local $8)
                                                        )
                                                      )
                                                      (get_local $35)
                                                    )
                                                  )
                                                  (get_local $35)
                                                )
                                                (block
                                                  (i32.store8
                                                    (get_local $37)
                                                    (i32.const 48)
                                                  )
                                                  (set_local $1
                                                    (get_local $37)
                                                  )
                                                )
                                              )
                                              (block $do-once115
                                                (if
                                                  (i32.eq
                                                    (get_local $8)
                                                    (get_local $13)
                                                  )
                                                  (block
                                                    (if
                                                      (i32.eqz
                                                        (i32.and
                                                          (i32.load
                                                            (get_local $0)
                                                          )
                                                          (i32.const 32)
                                                        )
                                                      )
                                                      (drop
                                                        (call $___fwritex
                                                          (get_local $1)
                                                          (i32.const 1)
                                                          (get_local $0)
                                                        )
                                                      )
                                                    )
                                                    (set_local $1
                                                      (i32.add
                                                        (get_local $1)
                                                        (i32.const 1)
                                                      )
                                                    )
                                                    (br_if $do-once115
                                                      (i32.and
                                                        (get_local $21)
                                                        (i32.lt_s
                                                          (get_local $5)
                                                          (i32.const 1)
                                                        )
                                                      )
                                                    )
                                                    (br_if $do-once115
                                                      (i32.and
                                                        (i32.load
                                                          (get_local $0)
                                                        )
                                                        (i32.const 32)
                                                      )
                                                    )
                                                    (drop
                                                      (call $___fwritex
                                                        (i32.const 3236)
                                                        (i32.const 1)
                                                        (get_local $0)
                                                      )
                                                    )
                                                  )
                                                  (block
                                                    (br_if $do-once115
                                                      (i32.le_u
                                                        (get_local $1)
                                                        (get_local $22)
                                                      )
                                                    )
                                                    (drop
                                                      (call $_memset
                                                        (get_local $22)
                                                        (i32.const 48)
                                                        (i32.add
                                                          (get_local $1)
                                                          (get_local $47)
                                                        )
                                                      )
                                                    )
                                                    (loop $while-in118
                                                      (br_if $while-in118
                                                        (i32.gt_u
                                                          (tee_local $1
                                                            (i32.add
                                                              (get_local $1)
                                                              (i32.const -1)
                                                            )
                                                          )
                                                          (get_local $22)
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                              (set_local $7
                                                (i32.sub
                                                  (get_local $45)
                                                  (get_local $1)
                                                )
                                              )
                                              (if
                                                (i32.eqz
                                                  (i32.and
                                                    (i32.load
                                                      (get_local $0)
                                                    )
                                                    (i32.const 32)
                                                  )
                                                )
                                                (drop
                                                  (call $___fwritex
                                                    (get_local $1)
                                                    (if i32
                                                      (i32.gt_s
                                                        (get_local $5)
                                                        (get_local $7)
                                                      )
                                                      (get_local $7)
                                                      (get_local $5)
                                                    )
                                                    (get_local $0)
                                                  )
                                                )
                                              )
                                              (br_if $while-in114
                                                (i32.and
                                                  (i32.lt_u
                                                    (tee_local $8
                                                      (i32.add
                                                        (get_local $8)
                                                        (i32.const 4)
                                                      )
                                                    )
                                                    (get_local $6)
                                                  )
                                                  (i32.gt_s
                                                    (tee_local $5
                                                      (i32.sub
                                                        (get_local $5)
                                                        (get_local $7)
                                                      )
                                                    )
                                                    (i32.const -1)
                                                  )
                                                )
                                              )
                                              (set_local $1
                                                (get_local $5)
                                              )
                                            )
                                          )
                                        )
                                        (call $_pad
                                          (get_local $0)
                                          (i32.const 48)
                                          (i32.add
                                            (get_local $1)
                                            (i32.const 18)
                                          )
                                          (i32.const 18)
                                          (i32.const 0)
                                        )
                                        (br_if $do-once99
                                          (i32.and
                                            (i32.load
                                              (get_local $0)
                                            )
                                            (i32.const 32)
                                          )
                                        )
                                        (drop
                                          (call $___fwritex
                                            (get_local $9)
                                            (i32.sub
                                              (get_local $31)
                                              (get_local $9)
                                            )
                                            (get_local $0)
                                          )
                                        )
                                      )
                                    )
                                  )
                                  (call $_pad
                                    (get_local $0)
                                    (i32.const 32)
                                    (get_local $10)
                                    (get_local $24)
                                    (i32.xor
                                      (get_local $12)
                                      (i32.const 8192)
                                    )
                                  )
                                  (if
                                    (i32.ge_s
                                      (get_local $24)
                                      (get_local $10)
                                    )
                                    (set_local $10
                                      (get_local $24)
                                    )
                                  )
                                )
                                (block
                                  (call $_pad
                                    (get_local $0)
                                    (i32.const 32)
                                    (get_local $10)
                                    (tee_local $8
                                      (i32.add
                                        (if i32
                                          (tee_local $7
                                            (i32.or
                                              (f64.ne
                                                (get_local $14)
                                                (get_local $14)
                                              )
                                              (i32.const 0)
                                            )
                                          )
                                          (tee_local $21
                                            (i32.const 0)
                                          )
                                          (get_local $21)
                                        )
                                        (i32.const 3)
                                      )
                                    )
                                    (get_local $6)
                                  )
                                  (if
                                    (i32.eqz
                                      (i32.and
                                        (tee_local $1
                                          (i32.load
                                            (get_local $0)
                                          )
                                        )
                                        (i32.const 32)
                                      )
                                    )
                                    (block
                                      (drop
                                        (call $___fwritex
                                          (get_local $29)
                                          (get_local $21)
                                          (get_local $0)
                                        )
                                      )
                                      (set_local $1
                                        (i32.load
                                          (get_local $0)
                                        )
                                      )
                                    )
                                  )
                                  (set_local $6
                                    (if i32
                                      (tee_local $5
                                        (i32.ne
                                          (i32.and
                                            (get_local $9)
                                            (i32.const 32)
                                          )
                                          (i32.const 0)
                                        )
                                      )
                                      (i32.const 3220)
                                      (i32.const 3224)
                                    )
                                  )
                                  (set_local $5
                                    (if i32
                                      (get_local $5)
                                      (i32.const 3228)
                                      (i32.const 3232)
                                    )
                                  )
                                  (if
                                    (i32.eqz
                                      (get_local $7)
                                    )
                                    (set_local $5
                                      (get_local $6)
                                    )
                                  )
                                  (if
                                    (i32.eqz
                                      (i32.and
                                        (get_local $1)
                                        (i32.const 32)
                                      )
                                    )
                                    (drop
                                      (call $___fwritex
                                        (get_local $5)
                                        (i32.const 3)
                                        (get_local $0)
                                      )
                                    )
                                  )
                                  (call $_pad
                                    (get_local $0)
                                    (i32.const 32)
                                    (get_local $10)
                                    (get_local $8)
                                    (i32.xor
                                      (get_local $12)
                                      (i32.const 8192)
                                    )
                                  )
                                  (if
                                    (i32.ge_s
                                      (get_local $8)
                                      (get_local $10)
                                    )
                                    (set_local $10
                                      (get_local $8)
                                    )
                                  )
                                )
                              )
                            )
                            (set_local $1
                              (get_local $11)
                            )
                            (br $label$continue$L1)
                          )
                          (set_local $7
                            (i32.const 0)
                          )
                          (set_local $9
                            (i32.const 3184)
                          )
                          (set_local $8
                            (get_local $25)
                          )
                          (br $__rjto$8)
                        )
                        (set_local $8
                          (i32.and
                            (get_local $9)
                            (i32.const 32)
                          )
                        )
                        (set_local $6
                          (if i32
                            (i64.eq
                              (tee_local $20
                                (i64.load
                                  (get_local $16)
                                )
                              )
                              (i64.const 0)
                            )
                            (block i32
                              (set_local $20
                                (i64.const 0)
                              )
                              (get_local $25)
                            )
                            (block i32
                              (set_local $1
                                (get_local $25)
                              )
                              (loop $while-in123
                                (i32.store8
                                  (tee_local $1
                                    (i32.add
                                      (get_local $1)
                                      (i32.const -1)
                                    )
                                  )
                                  (i32.or
                                    (i32.load8_u
                                      (i32.add
                                        (i32.and
                                          (i32.wrap/i64
                                            (get_local $20)
                                          )
                                          (i32.const 15)
                                        )
                                        (i32.const 3168)
                                      )
                                    )
                                    (get_local $8)
                                  )
                                )
                                (br_if $while-in123
                                  (i64.ne
                                    (tee_local $20
                                      (i64.shr_u
                                        (get_local $20)
                                        (i64.const 4)
                                      )
                                    )
                                    (i64.const 0)
                                  )
                                )
                              )
                              (set_local $20
                                (i64.load
                                  (get_local $16)
                                )
                              )
                              (get_local $1)
                            )
                          )
                        )
                        (set_local $8
                          (i32.add
                            (i32.shr_s
                              (get_local $9)
                              (i32.const 4)
                            )
                            (i32.const 3184)
                          )
                        )
                        (if
                          (tee_local $1
                            (i32.or
                              (i32.eqz
                                (i32.and
                                  (get_local $12)
                                  (i32.const 8)
                                )
                              )
                              (i64.eq
                                (get_local $20)
                                (i64.const 0)
                              )
                            )
                          )
                          (set_local $8
                            (i32.const 3184)
                          )
                        )
                        (set_local $7
                          (if i32
                            (get_local $1)
                            (i32.const 0)
                            (i32.const 2)
                          )
                        )
                        (br $__rjti$8)
                      )
                      (set_local $6
                        (call $_fmt_u
                          (get_local $20)
                          (get_local $25)
                        )
                      )
                      (br $__rjti$8)
                    )
                    (set_local $13
                      (i32.eqz
                        (tee_local $12
                          (call $_memchr
                            (get_local $1)
                            (i32.const 0)
                            (get_local $5)
                          )
                        )
                      )
                    )
                    (set_local $7
                      (i32.sub
                        (get_local $12)
                        (get_local $1)
                      )
                    )
                    (set_local $8
                      (i32.add
                        (get_local $1)
                        (get_local $5)
                      )
                    )
                    (if
                      (i32.eqz
                        (get_local $13)
                      )
                      (set_local $5
                        (get_local $7)
                      )
                    )
                    (set_local $7
                      (i32.const 0)
                    )
                    (set_local $9
                      (i32.const 3184)
                    )
                    (if
                      (i32.eqz
                        (get_local $13)
                      )
                      (set_local $8
                        (get_local $12)
                      )
                    )
                    (set_local $12
                      (get_local $6)
                    )
                    (br $__rjto$8)
                  )
                  (set_local $6
                    (get_local $5)
                  )
                  (set_local $1
                    (i32.const 0)
                  )
                  (set_local $8
                    (i32.const 0)
                  )
                  (loop $while-in125
                    (block $while-out124
                      (br_if $while-out124
                        (i32.eqz
                          (tee_local $9
                            (i32.load
                              (get_local $6)
                            )
                          )
                        )
                      )
                      (br_if $while-out124
                        (i32.or
                          (i32.lt_s
                            (tee_local $8
                              (call $_wctomb
                                (get_local $38)
                                (get_local $9)
                              )
                            )
                            (i32.const 0)
                          )
                          (i32.gt_u
                            (get_local $8)
                            (i32.sub
                              (get_local $7)
                              (get_local $1)
                            )
                          )
                        )
                      )
                      (set_local $6
                        (i32.add
                          (get_local $6)
                          (i32.const 4)
                        )
                      )
                      (br_if $while-in125
                        (i32.gt_u
                          (get_local $7)
                          (tee_local $1
                            (i32.add
                              (get_local $8)
                              (get_local $1)
                            )
                          )
                        )
                      )
                    )
                  )
                  (if
                    (i32.lt_s
                      (get_local $8)
                      (i32.const 0)
                    )
                    (block
                      (set_local $15
                        (i32.const -1)
                      )
                      (br $label$break$L1)
                    )
                  )
                  (call $_pad
                    (get_local $0)
                    (i32.const 32)
                    (get_local $10)
                    (get_local $1)
                    (get_local $12)
                  )
                  (if
                    (get_local $1)
                    (block
                      (set_local $8
                        (i32.const 0)
                      )
                      (loop $while-in127
                        (br_if $__rjti$7
                          (i32.eqz
                            (tee_local $6
                              (i32.load
                                (get_local $5)
                              )
                            )
                          )
                        )
                        (br_if $__rjti$7
                          (i32.gt_s
                            (tee_local $8
                              (i32.add
                                (tee_local $6
                                  (call $_wctomb
                                    (get_local $38)
                                    (get_local $6)
                                  )
                                )
                                (get_local $8)
                              )
                            )
                            (get_local $1)
                          )
                        )
                        (if
                          (i32.eqz
                            (i32.and
                              (i32.load
                                (get_local $0)
                              )
                              (i32.const 32)
                            )
                          )
                          (drop
                            (call $___fwritex
                              (get_local $38)
                              (get_local $6)
                              (get_local $0)
                            )
                          )
                        )
                        (set_local $5
                          (i32.add
                            (get_local $5)
                            (i32.const 4)
                          )
                        )
                        (br_if $while-in127
                          (i32.lt_u
                            (get_local $8)
                            (get_local $1)
                          )
                        )
                        (br $__rjti$7)
                      )
                    )
                    (block
                      (set_local $1
                        (i32.const 0)
                      )
                      (br $__rjti$7)
                    )
                  )
                )
                (call $_pad
                  (get_local $0)
                  (i32.const 32)
                  (get_local $10)
                  (get_local $1)
                  (i32.xor
                    (get_local $12)
                    (i32.const 8192)
                  )
                )
                (if
                  (i32.le_s
                    (get_local $10)
                    (get_local $1)
                  )
                  (set_local $10
                    (get_local $1)
                  )
                )
                (set_local $1
                  (get_local $11)
                )
                (br $label$continue$L1)
              )
              (set_local $1
                (i32.and
                  (get_local $12)
                  (i32.const -65537)
                )
              )
              (if
                (i32.gt_s
                  (get_local $5)
                  (i32.const -1)
                )
                (set_local $12
                  (get_local $1)
                )
              )
              (if
                (i32.or
                  (get_local $5)
                  (tee_local $13
                    (i64.ne
                      (i64.load
                        (get_local $16)
                      )
                      (i64.const 0)
                    )
                  )
                )
                (block
                  (set_local $1
                    (get_local $6)
                  )
                  (set_local $9
                    (get_local $8)
                  )
                  (set_local $8
                    (get_local $25)
                  )
                  (if
                    (i32.le_s
                      (get_local $5)
                      (tee_local $6
                        (i32.add
                          (i32.xor
                            (i32.and
                              (get_local $13)
                              (i32.const 1)
                            )
                            (i32.const 1)
                          )
                          (i32.sub
                            (get_local $42)
                            (get_local $6)
                          )
                        )
                      )
                    )
                    (set_local $5
                      (get_local $6)
                    )
                  )
                )
                (block
                  (set_local $1
                    (get_local $25)
                  )
                  (set_local $9
                    (get_local $8)
                  )
                  (set_local $8
                    (get_local $25)
                  )
                  (set_local $5
                    (i32.const 0)
                  )
                )
              )
            )
            (call $_pad
              (get_local $0)
              (i32.const 32)
              (if i32
                (i32.lt_s
                  (get_local $10)
                  (tee_local $5
                    (i32.add
                      (tee_local $6
                        (if i32
                          (i32.lt_s
                            (get_local $5)
                            (tee_local $8
                              (i32.sub
                                (get_local $8)
                                (get_local $1)
                              )
                            )
                          )
                          (get_local $8)
                          (get_local $5)
                        )
                      )
                      (get_local $7)
                    )
                  )
                )
                (tee_local $10
                  (get_local $5)
                )
                (get_local $10)
              )
              (get_local $5)
              (get_local $12)
            )
            (if
              (i32.eqz
                (i32.and
                  (i32.load
                    (get_local $0)
                  )
                  (i32.const 32)
                )
              )
              (drop
                (call $___fwritex
                  (get_local $9)
                  (get_local $7)
                  (get_local $0)
                )
              )
            )
            (call $_pad
              (get_local $0)
              (i32.const 48)
              (get_local $10)
              (get_local $5)
              (i32.xor
                (get_local $12)
                (i32.const 65536)
              )
            )
            (call $_pad
              (get_local $0)
              (i32.const 48)
              (get_local $6)
              (get_local $8)
              (i32.const 0)
            )
            (if
              (i32.eqz
                (i32.and
                  (i32.load
                    (get_local $0)
                  )
                  (i32.const 32)
                )
              )
              (drop
                (call $___fwritex
                  (get_local $1)
                  (get_local $8)
                  (get_local $0)
                )
              )
            )
            (call $_pad
              (get_local $0)
              (i32.const 32)
              (get_local $10)
              (get_local $5)
              (i32.xor
                (get_local $12)
                (i32.const 8192)
              )
            )
            (set_local $1
              (get_local $11)
            )
            (br $label$continue$L1)
          )
        )
        (br $label$break$L345)
      )
      (if
        (i32.eqz
          (get_local $0)
        )
        (if
          (get_local $17)
          (block
            (set_local $0
              (i32.const 1)
            )
            (loop $while-in130
              (if
                (tee_local $1
                  (i32.load
                    (i32.add
                      (get_local $4)
                      (i32.shl
                        (get_local $0)
                        (i32.const 2)
                      )
                    )
                  )
                )
                (block
                  (call $_pop_arg
                    (i32.add
                      (get_local $3)
                      (i32.shl
                        (get_local $0)
                        (i32.const 3)
                      )
                    )
                    (get_local $1)
                    (get_local $2)
                  )
                  (br_if $while-in130
                    (i32.lt_s
                      (tee_local $0
                        (i32.add
                          (get_local $0)
                          (i32.const 1)
                        )
                      )
                      (i32.const 10)
                    )
                  )
                  (set_local $15
                    (i32.const 1)
                  )
                  (br $label$break$L345)
                )
              )
            )
            (loop $while-in132
              (if
                (i32.load
                  (i32.add
                    (get_local $4)
                    (i32.shl
                      (get_local $0)
                      (i32.const 2)
                    )
                  )
                )
                (block
                  (set_local $15
                    (i32.const -1)
                  )
                  (br $label$break$L345)
                )
              )
              (br_if $while-in132
                (i32.lt_s
                  (tee_local $0
                    (i32.add
                      (get_local $0)
                      (i32.const 1)
                    )
                  )
                  (i32.const 10)
                )
              )
              (set_local $15
                (i32.const 1)
              )
            )
          )
          (set_local $15
            (i32.const 0)
          )
        )
      )
    )
    (set_global $STACKTOP
      (get_local $27)
    )
    (get_local $15)
  )
  (func $___lockfile (param $0 i32) (result i32)
    (i32.const 0)
  )
  (func $___fwritex (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
    (local $3 i32)
    (local $4 i32)
    (local $5 i32)
    (local $6 i32)
    (block $label$break$L5
      (block $__rjti$0
        (br_if $__rjti$0
          (tee_local $3
            (i32.load
              (tee_local $4
                (i32.add
                  (get_local $2)
                  (i32.const 16)
                )
              )
            )
          )
        )
        (if
          (call $___towrite
            (get_local $2)
          )
          (set_local $3
            (i32.const 0)
          )
          (block
            (set_local $3
              (i32.load
                (get_local $4)
              )
            )
            (br $__rjti$0)
          )
        )
        (br $label$break$L5)
      )
      (if
        (i32.lt_u
          (i32.sub
            (get_local $3)
            (tee_local $4
              (i32.load
                (tee_local $5
                  (i32.add
                    (get_local $2)
                    (i32.const 20)
                  )
                )
              )
            )
          )
          (get_local $1)
        )
        (block
          (set_local $3
            (call_indirect $FUNCSIG$iiii
              (get_local $2)
              (get_local $0)
              (get_local $1)
              (i32.add
                (i32.and
                  (i32.load offset=36
                    (get_local $2)
                  )
                  (i32.const 7)
                )
                (i32.const 0)
              )
            )
          )
          (br $label$break$L5)
        )
      )
      (block $label$break$L10
        (if
          (i32.gt_s
            (i32.load8_s offset=75
              (get_local $2)
            )
            (i32.const -1)
          )
          (block
            (set_local $3
              (get_local $1)
            )
            (loop $while-in
              (if
                (i32.eqz
                  (get_local $3)
                )
                (block
                  (set_local $3
                    (i32.const 0)
                  )
                  (br $label$break$L10)
                )
              )
              (if
                (i32.ne
                  (i32.load8_s
                    (i32.add
                      (get_local $0)
                      (tee_local $6
                        (i32.add
                          (get_local $3)
                          (i32.const -1)
                        )
                      )
                    )
                  )
                  (i32.const 10)
                )
                (block
                  (set_local $3
                    (get_local $6)
                  )
                  (br $while-in)
                )
              )
            )
            (br_if $label$break$L5
              (i32.lt_u
                (call_indirect $FUNCSIG$iiii
                  (get_local $2)
                  (get_local $0)
                  (get_local $3)
                  (i32.add
                    (i32.and
                      (i32.load offset=36
                        (get_local $2)
                      )
                      (i32.const 7)
                    )
                    (i32.const 0)
                  )
                )
                (get_local $3)
              )
            )
            (set_local $1
              (i32.sub
                (get_local $1)
                (get_local $3)
              )
            )
            (set_local $0
              (i32.add
                (get_local $0)
                (get_local $3)
              )
            )
            (set_local $4
              (i32.load
                (get_local $5)
              )
            )
          )
          (set_local $3
            (i32.const 0)
          )
        )
      )
      (drop
        (call $_memcpy
          (get_local $4)
          (get_local $0)
          (get_local $1)
        )
      )
      (i32.store
        (get_local $5)
        (i32.add
          (i32.load
            (get_local $5)
          )
          (get_local $1)
        )
      )
      (set_local $3
        (i32.add
          (get_local $3)
          (get_local $1)
        )
      )
    )
    (get_local $3)
  )
  (func $_pop_arg (param $0 i32) (param $1 i32) (param $2 i32)
    (local $3 i32)
    (local $4 f64)
    (local $5 i64)
    (block $label$break$L1
      (if
        (i32.le_u
          (get_local $1)
          (i32.const 20)
        )
        (block $switch-default
          (block $switch-case9
            (block $switch-case8
              (block $switch-case7
                (block $switch-case6
                  (block $switch-case5
                    (block $switch-case4
                      (block $switch-case3
                        (block $switch-case2
                          (block $switch-case1
                            (block $switch-case
                              (br_table $switch-case $switch-case1 $switch-case2 $switch-case3 $switch-case4 $switch-case5 $switch-case6 $switch-case7 $switch-case8 $switch-case9 $switch-default
                                (i32.sub
                                  (get_local $1)
                                  (i32.const 9)
                                )
                              )
                            )
                            (set_local $3
                              (i32.load
                                (tee_local $1
                                  (i32.and
                                    (i32.add
                                      (i32.load
                                        (get_local $2)
                                      )
                                      (i32.const 3)
                                    )
                                    (i32.const -4)
                                  )
                                )
                              )
                            )
                            (i32.store
                              (get_local $2)
                              (i32.add
                                (get_local $1)
                                (i32.const 4)
                              )
                            )
                            (i32.store
                              (get_local $0)
                              (get_local $3)
                            )
                            (br $label$break$L1)
                          )
                          (set_local $3
                            (i32.load
                              (tee_local $1
                                (i32.and
                                  (i32.add
                                    (i32.load
                                      (get_local $2)
                                    )
                                    (i32.const 3)
                                  )
                                  (i32.const -4)
                                )
                              )
                            )
                          )
                          (i32.store
                            (get_local $2)
                            (i32.add
                              (get_local $1)
                              (i32.const 4)
                            )
                          )
                          (i64.store
                            (get_local $0)
                            (i64.extend_s/i32
                              (get_local $3)
                            )
                          )
                          (br $label$break$L1)
                        )
                        (set_local $3
                          (i32.load
                            (tee_local $1
                              (i32.and
                                (i32.add
                                  (i32.load
                                    (get_local $2)
                                  )
                                  (i32.const 3)
                                )
                                (i32.const -4)
                              )
                            )
                          )
                        )
                        (i32.store
                          (get_local $2)
                          (i32.add
                            (get_local $1)
                            (i32.const 4)
                          )
                        )
                        (i64.store
                          (get_local $0)
                          (i64.extend_u/i32
                            (get_local $3)
                          )
                        )
                        (br $label$break$L1)
                      )
                      (set_local $5
                        (i64.load
                          (tee_local $1
                            (i32.and
                              (i32.add
                                (i32.load
                                  (get_local $2)
                                )
                                (i32.const 7)
                              )
                              (i32.const -8)
                            )
                          )
                        )
                      )
                      (i32.store
                        (get_local $2)
                        (i32.add
                          (get_local $1)
                          (i32.const 8)
                        )
                      )
                      (i64.store
                        (get_local $0)
                        (get_local $5)
                      )
                      (br $label$break$L1)
                    )
                    (set_local $3
                      (i32.load
                        (tee_local $1
                          (i32.and
                            (i32.add
                              (i32.load
                                (get_local $2)
                              )
                              (i32.const 3)
                            )
                            (i32.const -4)
                          )
                        )
                      )
                    )
                    (i32.store
                      (get_local $2)
                      (i32.add
                        (get_local $1)
                        (i32.const 4)
                      )
                    )
                    (i64.store
                      (get_local $0)
                      (i64.extend_s/i32
                        (i32.shr_s
                          (i32.shl
                            (i32.and
                              (get_local $3)
                              (i32.const 65535)
                            )
                            (i32.const 16)
                          )
                          (i32.const 16)
                        )
                      )
                    )
                    (br $label$break$L1)
                  )
                  (set_local $3
                    (i32.load
                      (tee_local $1
                        (i32.and
                          (i32.add
                            (i32.load
                              (get_local $2)
                            )
                            (i32.const 3)
                          )
                          (i32.const -4)
                        )
                      )
                    )
                  )
                  (i32.store
                    (get_local $2)
                    (i32.add
                      (get_local $1)
                      (i32.const 4)
                    )
                  )
                  (i64.store
                    (get_local $0)
                    (i64.extend_u/i32
                      (i32.and
                        (get_local $3)
                        (i32.const 65535)
                      )
                    )
                  )
                  (br $label$break$L1)
                )
                (set_local $3
                  (i32.load
                    (tee_local $1
                      (i32.and
                        (i32.add
                          (i32.load
                            (get_local $2)
                          )
                          (i32.const 3)
                        )
                        (i32.const -4)
                      )
                    )
                  )
                )
                (i32.store
                  (get_local $2)
                  (i32.add
                    (get_local $1)
                    (i32.const 4)
                  )
                )
                (i64.store
                  (get_local $0)
                  (i64.extend_s/i32
                    (i32.shr_s
                      (i32.shl
                        (i32.and
                          (get_local $3)
                          (i32.const 255)
                        )
                        (i32.const 24)
                      )
                      (i32.const 24)
                    )
                  )
                )
                (br $label$break$L1)
              )
              (set_local $3
                (i32.load
                  (tee_local $1
                    (i32.and
                      (i32.add
                        (i32.load
                          (get_local $2)
                        )
                        (i32.const 3)
                      )
                      (i32.const -4)
                    )
                  )
                )
              )
              (i32.store
                (get_local $2)
                (i32.add
                  (get_local $1)
                  (i32.const 4)
                )
              )
              (i64.store
                (get_local $0)
                (i64.extend_u/i32
                  (i32.and
                    (get_local $3)
                    (i32.const 255)
                  )
                )
              )
              (br $label$break$L1)
            )
            (set_local $4
              (f64.load
                (tee_local $1
                  (i32.and
                    (i32.add
                      (i32.load
                        (get_local $2)
                      )
                      (i32.const 7)
                    )
                    (i32.const -8)
                  )
                )
              )
            )
            (i32.store
              (get_local $2)
              (i32.add
                (get_local $1)
                (i32.const 8)
              )
            )
            (f64.store
              (get_local $0)
              (get_local $4)
            )
            (br $label$break$L1)
          )
          (set_local $4
            (f64.load
              (tee_local $1
                (i32.and
                  (i32.add
                    (i32.load
                      (get_local $2)
                    )
                    (i32.const 7)
                  )
                  (i32.const -8)
                )
              )
            )
          )
          (i32.store
            (get_local $2)
            (i32.add
              (get_local $1)
              (i32.const 8)
            )
          )
          (f64.store
            (get_local $0)
            (get_local $4)
          )
        )
      )
    )
  )
  (func $_fmt_u (param $0 i64) (param $1 i32) (result i32)
    (local $2 i32)
    (local $3 i64)
    (local $4 i32)
    (set_local $2
      (i32.wrap/i64
        (get_local $0)
      )
    )
    (if
      (i64.gt_u
        (get_local $0)
        (i64.const 4294967295)
      )
      (block
        (loop $while-in
          (i64.store8
            (tee_local $1
              (i32.add
                (get_local $1)
                (i32.const -1)
              )
            )
            (i64.or
              (call $i64u-rem
                (get_local $0)
                (i64.const 10)
              )
              (i64.const 48)
            )
          )
          (set_local $3
            (call $i64u-div
              (get_local $0)
              (i64.const 10)
            )
          )
          (if
            (i64.gt_u
              (get_local $0)
              (i64.const 42949672959)
            )
            (block
              (set_local $0
                (get_local $3)
              )
              (br $while-in)
            )
          )
        )
        (set_local $2
          (i32.wrap/i64
            (get_local $3)
          )
        )
      )
    )
    (if
      (get_local $2)
      (loop $while-in1
        (i32.store8
          (tee_local $1
            (i32.add
              (get_local $1)
              (i32.const -1)
            )
          )
          (i32.or
            (call $i32u-rem
              (get_local $2)
              (i32.const 10)
            )
            (i32.const 48)
          )
        )
        (set_local $4
          (call $i32u-div
            (get_local $2)
            (i32.const 10)
          )
        )
        (if
          (i32.ge_u
            (get_local $2)
            (i32.const 10)
          )
          (block
            (set_local $2
              (get_local $4)
            )
            (br $while-in1)
          )
        )
      )
    )
    (get_local $1)
  )
  (func $_strerror (param $0 i32) (result i32)
    (local $1 i32)
    (local $2 i32)
    (set_local $1
      (i32.const 0)
    )
    (block $__rjto$1
      (block $__rjti$1
        (block $__rjti$0
          (loop $while-in
            (br_if $__rjti$0
              (i32.eq
                (i32.load8_u
                  (i32.add
                    (get_local $1)
                    (i32.const 3238)
                  )
                )
                (get_local $0)
              )
            )
            (br_if $while-in
              (i32.ne
                (tee_local $1
                  (i32.add
                    (get_local $1)
                    (i32.const 1)
                  )
                )
                (i32.const 87)
              )
            )
            (set_local $0
              (i32.const 3326)
            )
            (set_local $1
              (i32.const 87)
            )
            (br $__rjti$1)
          )
        )
        (if
          (get_local $1)
          (block
            (set_local $0
              (i32.const 3326)
            )
            (br $__rjti$1)
          )
          (set_local $0
            (i32.const 3326)
          )
        )
        (br $__rjto$1)
      )
      (loop $while-in1
        (set_local $2
          (get_local $0)
        )
        (loop $while-in3
          (set_local $0
            (i32.add
              (get_local $2)
              (i32.const 1)
            )
          )
          (if
            (i32.load8_s
              (get_local $2)
            )
            (block
              (set_local $2
                (get_local $0)
              )
              (br $while-in3)
            )
          )
        )
        (br_if $while-in1
          (tee_local $1
            (i32.add
              (get_local $1)
              (i32.const -1)
            )
          )
        )
      )
    )
    (get_local $0)
  )
  (func $_memchr (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
    (local $3 i32)
    (local $4 i32)
    (local $5 i32)
    (set_local $4
      (i32.and
        (get_local $1)
        (i32.const 255)
      )
    )
    (block $label$break$L8
      (block $__rjti$2
        (block $__rjti$1
          (if
            (i32.and
              (tee_local $3
                (i32.ne
                  (get_local $2)
                  (i32.const 0)
                )
              )
              (i32.ne
                (i32.and
                  (get_local $0)
                  (i32.const 3)
                )
                (i32.const 0)
              )
            )
            (block
              (set_local $5
                (i32.and
                  (get_local $1)
                  (i32.const 255)
                )
              )
              (loop $while-in
                (br_if $__rjti$2
                  (i32.eq
                    (i32.load8_s
                      (get_local $0)
                    )
                    (i32.shr_s
                      (i32.shl
                        (get_local $5)
                        (i32.const 24)
                      )
                      (i32.const 24)
                    )
                  )
                )
                (br_if $while-in
                  (i32.and
                    (tee_local $3
                      (i32.ne
                        (tee_local $2
                          (i32.add
                            (get_local $2)
                            (i32.const -1)
                          )
                        )
                        (i32.const 0)
                      )
                    )
                    (i32.ne
                      (i32.and
                        (tee_local $0
                          (i32.add
                            (get_local $0)
                            (i32.const 1)
                          )
                        )
                        (i32.const 3)
                      )
                      (i32.const 0)
                    )
                  )
                )
                (br $__rjti$1)
              )
            )
          )
        )
        (br_if $__rjti$2
          (get_local $3)
        )
        (set_local $1
          (i32.const 0)
        )
        (br $label$break$L8)
      )
      (if
        (i32.eq
          (i32.load8_s
            (get_local $0)
          )
          (i32.shr_s
            (i32.shl
              (tee_local $3
                (i32.and
                  (get_local $1)
                  (i32.const 255)
                )
              )
              (i32.const 24)
            )
            (i32.const 24)
          )
        )
        (set_local $1
          (get_local $2)
        )
        (block
          (set_local $4
            (i32.mul
              (get_local $4)
              (i32.const 16843009)
            )
          )
          (block $__rjto$0
            (block $__rjti$0
              (if
                (i32.gt_u
                  (get_local $2)
                  (i32.const 3)
                )
                (block
                  (set_local $1
                    (get_local $2)
                  )
                  (loop $while-in3
                    (if
                      (i32.eqz
                        (i32.and
                          (i32.xor
                            (i32.and
                              (tee_local $2
                                (i32.xor
                                  (i32.load
                                    (get_local $0)
                                  )
                                  (get_local $4)
                                )
                              )
                              (i32.const -2139062144)
                            )
                            (i32.const -2139062144)
                          )
                          (i32.add
                            (get_local $2)
                            (i32.const -16843009)
                          )
                        )
                      )
                      (block
                        (set_local $0
                          (i32.add
                            (get_local $0)
                            (i32.const 4)
                          )
                        )
                        (br_if $while-in3
                          (i32.gt_u
                            (tee_local $1
                              (i32.add
                                (get_local $1)
                                (i32.const -4)
                              )
                            )
                            (i32.const 3)
                          )
                        )
                        (br $__rjti$0)
                      )
                    )
                  )
                )
                (block
                  (set_local $1
                    (get_local $2)
                  )
                  (br $__rjti$0)
                )
              )
              (br $__rjto$0)
            )
            (if
              (i32.eqz
                (get_local $1)
              )
              (block
                (set_local $1
                  (i32.const 0)
                )
                (br $label$break$L8)
              )
            )
          )
          (loop $while-in5
            (br_if $label$break$L8
              (i32.eq
                (i32.load8_s
                  (get_local $0)
                )
                (i32.shr_s
                  (i32.shl
                    (get_local $3)
                    (i32.const 24)
                  )
                  (i32.const 24)
                )
              )
            )
            (set_local $0
              (i32.add
                (get_local $0)
                (i32.const 1)
              )
            )
            (br_if $while-in5
              (tee_local $1
                (i32.add
                  (get_local $1)
                  (i32.const -1)
                )
              )
            )
            (set_local $1
              (i32.const 0)
            )
          )
        )
      )
    )
    (if i32
      (get_local $1)
      (get_local $0)
      (i32.const 0)
    )
  )
  (func $_pad (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32)
    (local $5 i32)
    (local $6 i32)
    (local $7 i32)
    (set_local $7
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 256)
      )
    )
    (set_local $6
      (get_local $7)
    )
    (block $do-once
      (if
        (i32.and
          (i32.gt_s
            (get_local $2)
            (get_local $3)
          )
          (i32.eqz
            (i32.and
              (get_local $4)
              (i32.const 73728)
            )
          )
        )
        (block
          (drop
            (call $_memset
              (get_local $6)
              (get_local $1)
              (if i32
                (i32.gt_u
                  (tee_local $5
                    (i32.sub
                      (get_local $2)
                      (get_local $3)
                    )
                  )
                  (i32.const 256)
                )
                (i32.const 256)
                (get_local $5)
              )
            )
          )
          (set_local $4
            (i32.eqz
              (i32.and
                (tee_local $1
                  (i32.load
                    (get_local $0)
                  )
                )
                (i32.const 32)
              )
            )
          )
          (if
            (i32.gt_u
              (get_local $5)
              (i32.const 255)
            )
            (block
              (loop $while-in
                (if
                  (get_local $4)
                  (block
                    (drop
                      (call $___fwritex
                        (get_local $6)
                        (i32.const 256)
                        (get_local $0)
                      )
                    )
                    (set_local $1
                      (i32.load
                        (get_local $0)
                      )
                    )
                  )
                )
                (set_local $4
                  (i32.eqz
                    (i32.and
                      (get_local $1)
                      (i32.const 32)
                    )
                  )
                )
                (br_if $while-in
                  (i32.gt_u
                    (tee_local $5
                      (i32.add
                        (get_local $5)
                        (i32.const -256)
                      )
                    )
                    (i32.const 255)
                  )
                )
              )
              (br_if $do-once
                (i32.eqz
                  (get_local $4)
                )
              )
              (set_local $5
                (i32.and
                  (i32.sub
                    (get_local $2)
                    (get_local $3)
                  )
                  (i32.const 255)
                )
              )
            )
            (br_if $do-once
              (i32.eqz
                (get_local $4)
              )
            )
          )
          (drop
            (call $___fwritex
              (get_local $6)
              (get_local $5)
              (get_local $0)
            )
          )
        )
      )
    )
    (set_global $STACKTOP
      (get_local $7)
    )
  )
  (func $_wctomb (param $0 i32) (param $1 i32) (result i32)
    (if i32
      (get_local $0)
      (call $_wcrtomb
        (get_local $0)
        (get_local $1)
        (i32.const 0)
      )
      (i32.const 0)
    )
  )
  (func $_frexpl (param $0 f64) (param $1 i32) (result f64)
    (call $_frexp
      (get_local $0)
      (get_local $1)
    )
  )
  (func $_frexp (param $0 f64) (param $1 i32) (result f64)
    (local $2 i64)
    (local $3 i64)
    (block $switch
      (block $switch-default
        (block $switch-case0
          (block $switch-case
            (br_table $switch-case $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-default $switch-case0 $switch-default
              (i32.sub
                (i32.shr_s
                  (i32.shl
                    (i32.and
                      (i32.and
                        (i32.wrap/i64
                          (tee_local $3
                            (i64.shr_u
                              (tee_local $2
                                (i64.reinterpret/f64
                                  (get_local $0)
                                )
                              )
                              (i64.const 52)
                            )
                          )
                        )
                        (i32.const 65535)
                      )
                      (i32.const 2047)
                    )
                    (i32.const 16)
                  )
                  (i32.const 16)
                )
                (i32.const 0)
              )
            )
          )
          (i32.store
            (get_local $1)
            (if i32
              (f64.ne
                (get_local $0)
                (f64.const 0)
              )
              (block i32
                (set_local $0
                  (call $_frexp
                    (f64.mul
                      (get_local $0)
                      (f64.const 18446744073709551615)
                    )
                    (get_local $1)
                  )
                )
                (i32.add
                  (i32.load
                    (get_local $1)
                  )
                  (i32.const -64)
                )
              )
              (i32.const 0)
            )
          )
          (br $switch)
        )
        (br $switch)
      )
      (i32.store
        (get_local $1)
        (i32.add
          (i32.and
            (i32.wrap/i64
              (get_local $3)
            )
            (i32.const 2047)
          )
          (i32.const -1022)
        )
      )
      (set_local $0
        (f64.reinterpret/i64
          (i64.or
            (i64.and
              (get_local $2)
              (i64.const -9218868437227405313)
            )
            (i64.const 4602678819172646912)
          )
        )
      )
    )
    (get_local $0)
  )
  (func $_wcrtomb (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
    (block $do-once i32
      (if i32
        (get_local $0)
        (block i32
          (if
            (i32.lt_u
              (get_local $1)
              (i32.const 128)
            )
            (block
              (i32.store8
                (get_local $0)
                (get_local $1)
              )
              (br $do-once
                (i32.const 1)
              )
            )
          )
          (if
            (i32.lt_u
              (get_local $1)
              (i32.const 2048)
            )
            (block
              (i32.store8
                (get_local $0)
                (i32.or
                  (i32.shr_u
                    (get_local $1)
                    (i32.const 6)
                  )
                  (i32.const 192)
                )
              )
              (i32.store8 offset=1
                (get_local $0)
                (i32.or
                  (i32.and
                    (get_local $1)
                    (i32.const 63)
                  )
                  (i32.const 128)
                )
              )
              (br $do-once
                (i32.const 2)
              )
            )
          )
          (if
            (i32.or
              (i32.lt_u
                (get_local $1)
                (i32.const 55296)
              )
              (i32.eq
                (i32.and
                  (get_local $1)
                  (i32.const -8192)
                )
                (i32.const 57344)
              )
            )
            (block
              (i32.store8
                (get_local $0)
                (i32.or
                  (i32.shr_u
                    (get_local $1)
                    (i32.const 12)
                  )
                  (i32.const 224)
                )
              )
              (i32.store8 offset=1
                (get_local $0)
                (i32.or
                  (i32.and
                    (i32.shr_u
                      (get_local $1)
                      (i32.const 6)
                    )
                    (i32.const 63)
                  )
                  (i32.const 128)
                )
              )
              (i32.store8 offset=2
                (get_local $0)
                (i32.or
                  (i32.and
                    (get_local $1)
                    (i32.const 63)
                  )
                  (i32.const 128)
                )
              )
              (br $do-once
                (i32.const 3)
              )
            )
          )
          (if i32
            (i32.lt_u
              (i32.add
                (get_local $1)
                (i32.const -65536)
              )
              (i32.const 1048576)
            )
            (block i32
              (i32.store8
                (get_local $0)
                (i32.or
                  (i32.shr_u
                    (get_local $1)
                    (i32.const 18)
                  )
                  (i32.const 240)
                )
              )
              (i32.store8 offset=1
                (get_local $0)
                (i32.or
                  (i32.and
                    (i32.shr_u
                      (get_local $1)
                      (i32.const 12)
                    )
                    (i32.const 63)
                  )
                  (i32.const 128)
                )
              )
              (i32.store8 offset=2
                (get_local $0)
                (i32.or
                  (i32.and
                    (i32.shr_u
                      (get_local $1)
                      (i32.const 6)
                    )
                    (i32.const 63)
                  )
                  (i32.const 128)
                )
              )
              (i32.store8 offset=3
                (get_local $0)
                (i32.or
                  (i32.and
                    (get_local $1)
                    (i32.const 63)
                  )
                  (i32.const 128)
                )
              )
              (i32.const 4)
            )
            (block i32
              (i32.store
                (call $___errno_location)
                (i32.const 84)
              )
              (i32.const -1)
            )
          )
        )
        (i32.const 1)
      )
    )
  )
  (func $___towrite (param $0 i32) (result i32)
    (local $1 i32)
    (local $2 i32)
    (set_local $1
      (i32.load8_s
        (tee_local $2
          (i32.add
            (get_local $0)
            (i32.const 74)
          )
        )
      )
    )
    (i32.store8
      (get_local $2)
      (i32.or
        (i32.add
          (get_local $1)
          (i32.const 255)
        )
        (get_local $1)
      )
    )
    (tee_local $0
      (if i32
        (i32.and
          (tee_local $1
            (i32.load
              (get_local $0)
            )
          )
          (i32.const 8)
        )
        (block i32
          (i32.store
            (get_local $0)
            (i32.or
              (get_local $1)
              (i32.const 32)
            )
          )
          (i32.const -1)
        )
        (block i32
          (i32.store offset=8
            (get_local $0)
            (i32.const 0)
          )
          (i32.store offset=4
            (get_local $0)
            (i32.const 0)
          )
          (i32.store offset=28
            (get_local $0)
            (tee_local $1
              (i32.load offset=44
                (get_local $0)
              )
            )
          )
          (i32.store offset=20
            (get_local $0)
            (get_local $1)
          )
          (i32.store offset=16
            (get_local $0)
            (i32.add
              (get_local $1)
              (i32.load offset=48
                (get_local $0)
              )
            )
          )
          (i32.const 0)
        )
      )
    )
  )
  (func $_sn_write (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
    (local $3 i32)
    (local $4 i32)
    (if
      (i32.gt_u
        (tee_local $0
          (i32.sub
            (i32.load offset=16
              (get_local $0)
            )
            (tee_local $4
              (i32.load
                (tee_local $3
                  (i32.add
                    (get_local $0)
                    (i32.const 20)
                  )
                )
              )
            )
          )
        )
        (get_local $2)
      )
      (set_local $0
        (get_local $2)
      )
    )
    (drop
      (call $_memcpy
        (get_local $4)
        (get_local $1)
        (get_local $0)
      )
    )
    (i32.store
      (get_local $3)
      (i32.add
        (i32.load
          (get_local $3)
        )
        (get_local $0)
      )
    )
    (get_local $2)
  )
  (func $_scalbn (param $0 f64) (param $1 i32) (result f64)
    (local $2 i32)
    (if
      (i32.gt_s
        (get_local $1)
        (i32.const 1023)
      )
      (block
        (set_local $0
          (f64.mul
            (get_local $0)
            (f64.const 8988465674311579538646525e283)
          )
        )
        (set_local $2
          (i32.add
            (get_local $1)
            (i32.const -2046)
          )
        )
        (if
          (i32.gt_s
            (tee_local $1
              (i32.add
                (get_local $1)
                (i32.const -1023)
              )
            )
            (i32.const 1023)
          )
          (block
            (set_local $0
              (f64.mul
                (get_local $0)
                (f64.const 8988465674311579538646525e283)
              )
            )
            (set_local $1
              (if i32
                (i32.gt_s
                  (get_local $2)
                  (i32.const 1023)
                )
                (i32.const 1023)
                (get_local $2)
              )
            )
          )
        )
      )
      (if
        (i32.lt_s
          (get_local $1)
          (i32.const -1022)
        )
        (block
          (set_local $0
            (f64.mul
              (get_local $0)
              (f64.const 2.2250738585072014e-308)
            )
          )
          (set_local $2
            (i32.add
              (get_local $1)
              (i32.const 2044)
            )
          )
          (if
            (i32.lt_s
              (tee_local $1
                (i32.add
                  (get_local $1)
                  (i32.const 1022)
                )
              )
              (i32.const -1022)
            )
            (block
              (set_local $0
                (f64.mul
                  (get_local $0)
                  (f64.const 2.2250738585072014e-308)
                )
              )
              (set_local $1
                (if i32
                  (i32.lt_s
                    (get_local $2)
                    (i32.const -1022)
                  )
                  (i32.const -1022)
                  (get_local $2)
                )
              )
            )
          )
        )
      )
    )
    (f64.mul
      (get_local $0)
      (f64.reinterpret/i64
        (i64.shl
          (i64.extend_u/i32
            (i32.add
              (get_local $1)
              (i32.const 1023)
            )
          )
          (i64.const 52)
        )
      )
    )
  )
  (func $___cos (param $0 f64) (param $1 f64) (result f64)
    (local $2 f64)
    (local $3 f64)
    (local $4 f64)
    (local $5 f64)
    (set_local $3
      (f64.mul
        (tee_local $2
          (f64.mul
            (get_local $0)
            (get_local $0)
          )
        )
        (get_local $2)
      )
    )
    (f64.add
      (tee_local $5
        (f64.sub
          (f64.const 1)
          (tee_local $4
            (f64.mul
              (get_local $2)
              (f64.const 0.5)
            )
          )
        )
      )
      (f64.add
        (f64.sub
          (f64.sub
            (f64.const 1)
            (get_local $5)
          )
          (get_local $4)
        )
        (f64.sub
          (f64.mul
            (get_local $2)
            (f64.add
              (f64.mul
                (get_local $2)
                (f64.add
                  (f64.mul
                    (get_local $2)
                    (f64.add
                      (f64.mul
                        (get_local $2)
                        (f64.const 2.480158728947673e-05)
                      )
                      (f64.const -0.001388888888887411)
                    )
                  )
                  (f64.const 0.0416666666666666)
                )
              )
              (f64.mul
                (f64.mul
                  (get_local $3)
                  (get_local $3)
                )
                (f64.add
                  (f64.mul
                    (get_local $2)
                    (f64.sub
                      (f64.const 2.087572321298175e-09)
                      (f64.mul
                        (get_local $2)
                        (f64.const 1.1359647557788195e-11)
                      )
                    )
                  )
                  (f64.const -2.7557314351390663e-07)
                )
              )
            )
          )
          (f64.mul
            (get_local $0)
            (get_local $1)
          )
        )
      )
    )
  )
  (func $___rem_pio2 (param $0 f64) (param $1 i32) (result i32)
    (local $2 f64)
    (local $3 i32)
    (local $4 i32)
    (local $5 f64)
    (local $6 f64)
    (local $7 i32)
    (local $8 i32)
    (local $9 i32)
    (local $10 i32)
    (local $11 i32)
    (local $12 i64)
    (set_local $9
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 48)
      )
    )
    (set_local $7
      (i32.add
        (get_local $9)
        (i32.const 16)
      )
    )
    (set_local $10
      (get_local $9)
    )
    (set_local $8
      (i32.wrap/i64
        (i64.shr_u
          (tee_local $12
            (i64.reinterpret/f64
              (get_local $0)
            )
          )
          (i64.const 63)
        )
      )
    )
    (set_local $1
      (block $__rjto$0 i32
        (block $__rjti$0
          (br $__rjto$0
            (if i32
              (i32.lt_u
                (tee_local $4
                  (i32.and
                    (tee_local $3
                      (i32.wrap/i64
                        (i64.shr_u
                          (get_local $12)
                          (i64.const 32)
                        )
                      )
                    )
                    (i32.const 2147483647)
                  )
                )
                (i32.const 1074752123)
              )
              (block i32
                (br_if $__rjti$0
                  (i32.eq
                    (i32.and
                      (get_local $3)
                      (i32.const 1048575)
                    )
                    (i32.const 598523)
                  )
                )
                (set_local $3
                  (i32.ne
                    (get_local $8)
                    (i32.const 0)
                  )
                )
                (if i32
                  (i32.lt_u
                    (get_local $4)
                    (i32.const 1073928573)
                  )
                  (if i32
                    (get_local $3)
                    (block i32
                      (f64.store
                        (get_local $1)
                        (tee_local $2
                          (f64.add
                            (tee_local $0
                              (f64.add
                                (get_local $0)
                                (f64.const 1.5707963267341256)
                              )
                            )
                            (f64.const 6.077100506506192e-11)
                          )
                        )
                      )
                      (f64.store offset=8
                        (get_local $1)
                        (f64.add
                          (f64.sub
                            (get_local $0)
                            (get_local $2)
                          )
                          (f64.const 6.077100506506192e-11)
                        )
                      )
                      (i32.const -1)
                    )
                    (block i32
                      (f64.store
                        (get_local $1)
                        (tee_local $2
                          (f64.add
                            (tee_local $0
                              (f64.add
                                (get_local $0)
                                (f64.const -1.5707963267341256)
                              )
                            )
                            (f64.const -6.077100506506192e-11)
                          )
                        )
                      )
                      (f64.store offset=8
                        (get_local $1)
                        (f64.add
                          (f64.sub
                            (get_local $0)
                            (get_local $2)
                          )
                          (f64.const -6.077100506506192e-11)
                        )
                      )
                      (i32.const 1)
                    )
                  )
                  (if i32
                    (get_local $3)
                    (block i32
                      (f64.store
                        (get_local $1)
                        (tee_local $2
                          (f64.add
                            (tee_local $0
                              (f64.add
                                (get_local $0)
                                (f64.const 3.1415926534682512)
                              )
                            )
                            (f64.const 1.2154201013012384e-10)
                          )
                        )
                      )
                      (f64.store offset=8
                        (get_local $1)
                        (f64.add
                          (f64.sub
                            (get_local $0)
                            (get_local $2)
                          )
                          (f64.const 1.2154201013012384e-10)
                        )
                      )
                      (i32.const -2)
                    )
                    (block i32
                      (f64.store
                        (get_local $1)
                        (tee_local $2
                          (f64.add
                            (tee_local $0
                              (f64.add
                                (get_local $0)
                                (f64.const -3.1415926534682512)
                              )
                            )
                            (f64.const -1.2154201013012384e-10)
                          )
                        )
                      )
                      (f64.store offset=8
                        (get_local $1)
                        (f64.add
                          (f64.sub
                            (get_local $0)
                            (get_local $2)
                          )
                          (f64.const -1.2154201013012384e-10)
                        )
                      )
                      (i32.const 2)
                    )
                  )
                )
              )
              (block i32
                (if
                  (i32.lt_u
                    (get_local $4)
                    (i32.const 1075594812)
                  )
                  (if
                    (i32.lt_u
                      (get_local $4)
                      (i32.const 1075183037)
                    )
                    (block
                      (br_if $__rjti$0
                        (i32.eq
                          (get_local $4)
                          (i32.const 1074977148)
                        )
                      )
                      (if
                        (get_local $8)
                        (block
                          (f64.store
                            (get_local $1)
                            (tee_local $2
                              (f64.add
                                (tee_local $0
                                  (f64.add
                                    (get_local $0)
                                    (f64.const 4.712388980202377)
                                  )
                                )
                                (f64.const 1.8231301519518578e-10)
                              )
                            )
                          )
                          (f64.store offset=8
                            (get_local $1)
                            (f64.add
                              (f64.sub
                                (get_local $0)
                                (get_local $2)
                              )
                              (f64.const 1.8231301519518578e-10)
                            )
                          )
                          (br $__rjto$0
                            (i32.const -3)
                          )
                        )
                        (block
                          (f64.store
                            (get_local $1)
                            (tee_local $2
                              (f64.add
                                (tee_local $0
                                  (f64.add
                                    (get_local $0)
                                    (f64.const -4.712388980202377)
                                  )
                                )
                                (f64.const -1.8231301519518578e-10)
                              )
                            )
                          )
                          (f64.store offset=8
                            (get_local $1)
                            (f64.add
                              (f64.sub
                                (get_local $0)
                                (get_local $2)
                              )
                              (f64.const -1.8231301519518578e-10)
                            )
                          )
                          (br $__rjto$0
                            (i32.const 3)
                          )
                        )
                      )
                    )
                    (block
                      (br_if $__rjti$0
                        (i32.eq
                          (get_local $4)
                          (i32.const 1075388923)
                        )
                      )
                      (if
                        (get_local $8)
                        (block
                          (f64.store
                            (get_local $1)
                            (tee_local $2
                              (f64.add
                                (tee_local $0
                                  (f64.add
                                    (get_local $0)
                                    (f64.const 6.2831853069365025)
                                  )
                                )
                                (f64.const 2.430840202602477e-10)
                              )
                            )
                          )
                          (f64.store offset=8
                            (get_local $1)
                            (f64.add
                              (f64.sub
                                (get_local $0)
                                (get_local $2)
                              )
                              (f64.const 2.430840202602477e-10)
                            )
                          )
                          (br $__rjto$0
                            (i32.const -4)
                          )
                        )
                        (block
                          (f64.store
                            (get_local $1)
                            (tee_local $2
                              (f64.add
                                (tee_local $0
                                  (f64.add
                                    (get_local $0)
                                    (f64.const -6.2831853069365025)
                                  )
                                )
                                (f64.const -2.430840202602477e-10)
                              )
                            )
                          )
                          (f64.store offset=8
                            (get_local $1)
                            (f64.add
                              (f64.sub
                                (get_local $0)
                                (get_local $2)
                              )
                              (f64.const -2.430840202602477e-10)
                            )
                          )
                          (br $__rjto$0
                            (i32.const 4)
                          )
                        )
                      )
                    )
                  )
                )
                (br_if $__rjti$0
                  (i32.lt_u
                    (get_local $4)
                    (i32.const 1094263291)
                  )
                )
                (if
                  (i32.gt_u
                    (get_local $4)
                    (i32.const 2146435071)
                  )
                  (block
                    (f64.store offset=8
                      (get_local $1)
                      (tee_local $0
                        (f64.sub
                          (get_local $0)
                          (get_local $0)
                        )
                      )
                    )
                    (f64.store
                      (get_local $1)
                      (get_local $0)
                    )
                    (br $__rjto$0
                      (i32.const 0)
                    )
                  )
                )
                (set_local $0
                  (f64.reinterpret/i64
                    (i64.or
                      (i64.and
                        (get_local $12)
                        (i64.const 4503599627370495)
                      )
                      (i64.const 4710765210229538816)
                    )
                  )
                )
                (set_local $3
                  (i32.const 0)
                )
                (loop $while-in
                  (f64.store
                    (i32.add
                      (get_local $7)
                      (i32.shl
                        (get_local $3)
                        (i32.const 3)
                      )
                    )
                    (tee_local $2
                      (f64.convert_s/i32
                        (call $f64-to-int
                          (get_local $0)
                        )
                      )
                    )
                  )
                  (set_local $0
                    (f64.mul
                      (f64.sub
                        (get_local $0)
                        (get_local $2)
                      )
                      (f64.const 16777216)
                    )
                  )
                  (br_if $while-in
                    (i32.ne
                      (tee_local $3
                        (i32.add
                          (get_local $3)
                          (i32.const 1)
                        )
                      )
                      (i32.const 2)
                    )
                  )
                )
                (f64.store offset=16
                  (get_local $7)
                  (get_local $0)
                )
                (if
                  (f64.eq
                    (get_local $0)
                    (f64.const 0)
                  )
                  (block
                    (set_local $3
                      (i32.const 1)
                    )
                    (loop $while-in1
                      (set_local $11
                        (i32.add
                          (get_local $3)
                          (i32.const -1)
                        )
                      )
                      (if
                        (f64.eq
                          (f64.load
                            (i32.add
                              (get_local $7)
                              (i32.shl
                                (get_local $3)
                                (i32.const 3)
                              )
                            )
                          )
                          (f64.const 0)
                        )
                        (block
                          (set_local $3
                            (get_local $11)
                          )
                          (br $while-in1)
                        )
                      )
                    )
                  )
                  (set_local $3
                    (i32.const 2)
                  )
                )
                (set_local $3
                  (call $___rem_pio2_large
                    (get_local $7)
                    (get_local $10)
                    (i32.add
                      (i32.shr_u
                        (get_local $4)
                        (i32.const 20)
                      )
                      (i32.const -1046)
                    )
                    (i32.add
                      (get_local $3)
                      (i32.const 1)
                    )
                    (i32.const 1)
                  )
                )
                (set_local $0
                  (f64.load
                    (get_local $10)
                  )
                )
                (set_local $2
                  (f64.load offset=8
                    (get_local $10)
                  )
                )
                (if i32
                  (get_local $8)
                  (block i32
                    (f64.store
                      (get_local $1)
                      (f64.neg
                        (get_local $0)
                      )
                    )
                    (f64.store offset=8
                      (get_local $1)
                      (f64.neg
                        (get_local $2)
                      )
                    )
                    (i32.sub
                      (i32.const 0)
                      (get_local $3)
                    )
                  )
                  (block i32
                    (f64.store
                      (get_local $1)
                      (get_local $0)
                    )
                    (f64.store offset=8
                      (get_local $1)
                      (get_local $2)
                    )
                    (get_local $3)
                  )
                )
              )
            )
          )
        )
        (set_local $3
          (call $f64-to-int
            (tee_local $5
              (f64.add
                (f64.add
                  (f64.mul
                    (get_local $0)
                    (f64.const 0.6366197723675814)
                  )
                  (f64.const 6755399441055744)
                )
                (f64.const -6755399441055744)
              )
            )
          )
        )
        (f64.store
          (get_local $1)
          (tee_local $6
            (f64.sub
              (tee_local $0
                (f64.sub
                  (get_local $0)
                  (f64.mul
                    (get_local $5)
                    (f64.const 1.5707963267341256)
                  )
                )
              )
              (tee_local $2
                (f64.mul
                  (get_local $5)
                  (f64.const 6.077100506506192e-11)
                )
              )
            )
          )
        )
        (if
          (i32.gt_s
            (i32.sub
              (tee_local $11
                (i32.shr_u
                  (get_local $4)
                  (i32.const 20)
                )
              )
              (i32.and
                (i32.wrap/i64
                  (i64.shr_u
                    (i64.reinterpret/f64
                      (get_local $6)
                    )
                    (i64.const 52)
                  )
                )
                (i32.const 2047)
              )
            )
            (i32.const 16)
          )
          (block
            (set_local $2
              (f64.sub
                (f64.mul
                  (get_local $5)
                  (f64.const 2.0222662487959506e-21)
                )
                (f64.sub
                  (f64.sub
                    (get_local $0)
                    (tee_local $0
                      (f64.sub
                        (get_local $0)
                        (tee_local $2
                          (f64.mul
                            (get_local $5)
                            (f64.const 6.077100506303966e-11)
                          )
                        )
                      )
                    )
                  )
                  (get_local $2)
                )
              )
            )
            (f64.store
              (get_local $1)
              (tee_local $6
                (f64.sub
                  (get_local $0)
                  (get_local $2)
                )
              )
            )
            (if
              (i32.gt_s
                (i32.sub
                  (get_local $11)
                  (i32.and
                    (i32.wrap/i64
                      (i64.shr_u
                        (i64.reinterpret/f64
                          (get_local $6)
                        )
                        (i64.const 52)
                      )
                    )
                    (i32.const 2047)
                  )
                )
                (i32.const 49)
              )
              (block
                (set_local $2
                  (f64.sub
                    (f64.mul
                      (get_local $5)
                      (f64.const 8.4784276603689e-32)
                    )
                    (f64.sub
                      (f64.sub
                        (get_local $0)
                        (tee_local $0
                          (f64.sub
                            (get_local $0)
                            (tee_local $2
                              (f64.mul
                                (get_local $5)
                                (f64.const 2.0222662487111665e-21)
                              )
                            )
                          )
                        )
                      )
                      (get_local $2)
                    )
                  )
                )
                (f64.store
                  (get_local $1)
                  (tee_local $6
                    (f64.sub
                      (get_local $0)
                      (get_local $2)
                    )
                  )
                )
              )
            )
          )
        )
        (f64.store offset=8
          (get_local $1)
          (f64.sub
            (f64.sub
              (get_local $0)
              (get_local $6)
            )
            (get_local $2)
          )
        )
        (get_local $3)
      )
    )
    (set_global $STACKTOP
      (get_local $9)
    )
    (get_local $1)
  )
  (func $___rem_pio2_large (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32) (result i32)
    (local $5 f64)
    (local $6 i32)
    (local $7 i32)
    (local $8 i32)
    (local $9 i32)
    (local $10 i32)
    (local $11 f64)
    (local $12 i32)
    (local $13 i32)
    (local $14 i32)
    (local $15 i32)
    (local $16 i32)
    (local $17 f64)
    (local $18 i32)
    (local $19 i32)
    (local $20 i32)
    (local $21 i32)
    (local $22 i32)
    (local $23 i32)
    (local $24 i32)
    (local $25 i32)
    (local $26 i32)
    (local $27 i32)
    (local $28 i32)
    (set_local $18
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 560)
      )
    )
    (set_local $20
      (i32.add
        (get_local $18)
        (i32.const 320)
      )
    )
    (set_local $15
      (i32.load
        (i32.add
          (i32.shl
            (get_local $4)
            (i32.const 2)
          )
          (i32.const 1960)
        )
      )
    )
    (set_local $7
      (i32.add
        (get_local $3)
        (i32.const -1)
      )
    )
    (if
      (i32.lt_s
        (tee_local $21
          (call $i32s-div
            (i32.add
              (get_local $2)
              (i32.const -3)
            )
            (i32.const 24)
          )
        )
        (i32.const 0)
      )
      (set_local $21
        (i32.const 0)
      )
    )
    (if
      (i32.ge_s
        (i32.add
          (get_local $15)
          (get_local $7)
        )
        (i32.const 0)
      )
      (block
        (set_local $10
          (i32.add
            (get_local $15)
            (get_local $3)
          )
        )
        (set_local $6
          (i32.sub
            (get_local $21)
            (get_local $7)
          )
        )
        (set_local $8
          (i32.const 0)
        )
        (loop $while-in
          (f64.store
            (i32.add
              (get_local $20)
              (i32.shl
                (get_local $8)
                (i32.const 3)
              )
            )
            (tee_local $5
              (if f64
                (i32.lt_s
                  (get_local $6)
                  (i32.const 0)
                )
                (f64.const 0)
                (f64.convert_s/i32
                  (i32.load
                    (i32.add
                      (i32.shl
                        (get_local $6)
                        (i32.const 2)
                      )
                      (i32.const 1976)
                    )
                  )
                )
              )
            )
          )
          (set_local $6
            (i32.add
              (get_local $6)
              (i32.const 1)
            )
          )
          (br_if $while-in
            (i32.ne
              (tee_local $8
                (i32.add
                  (get_local $8)
                  (i32.const 1)
                )
              )
              (get_local $10)
            )
          )
        )
      )
    )
    (set_local $14
      (i32.add
        (get_local $18)
        (i32.const 480)
      )
    )
    (set_local $12
      (i32.add
        (get_local $18)
        (i32.const 160)
      )
    )
    (set_local $16
      (get_local $18)
    )
    (set_local $10
      (i32.add
        (i32.add
          (get_local $2)
          (i32.const -24)
        )
        (tee_local $25
          (i32.mul
            (get_local $21)
            (i32.const -24)
          )
        )
      )
    )
    (set_local $9
      (i32.gt_s
        (get_local $3)
        (i32.const 0)
      )
    )
    (set_local $6
      (i32.const 0)
    )
    (loop $while-in1
      (if
        (get_local $9)
        (block
          (set_local $13
            (i32.add
              (get_local $6)
              (get_local $7)
            )
          )
          (set_local $5
            (f64.const 0)
          )
          (set_local $8
            (i32.const 0)
          )
          (loop $while-in3
            (set_local $5
              (f64.add
                (get_local $5)
                (f64.mul
                  (f64.load
                    (i32.add
                      (get_local $0)
                      (i32.shl
                        (get_local $8)
                        (i32.const 3)
                      )
                    )
                  )
                  (f64.load
                    (i32.add
                      (get_local $20)
                      (i32.shl
                        (i32.sub
                          (get_local $13)
                          (get_local $8)
                        )
                        (i32.const 3)
                      )
                    )
                  )
                )
              )
            )
            (br_if $while-in3
              (i32.ne
                (tee_local $8
                  (i32.add
                    (get_local $8)
                    (i32.const 1)
                  )
                )
                (get_local $3)
              )
            )
          )
        )
        (set_local $5
          (f64.const 0)
        )
      )
      (f64.store
        (i32.add
          (get_local $16)
          (i32.shl
            (get_local $6)
            (i32.const 3)
          )
        )
        (get_local $5)
      )
      (set_local $8
        (i32.add
          (get_local $6)
          (i32.const 1)
        )
      )
      (if
        (i32.lt_s
          (get_local $6)
          (get_local $15)
        )
        (block
          (set_local $6
            (get_local $8)
          )
          (br $while-in1)
        )
      )
    )
    (set_local $22
      (i32.gt_s
        (get_local $10)
        (i32.const 0)
      )
    )
    (set_local $23
      (i32.sub
        (i32.const 24)
        (get_local $10)
      )
    )
    (set_local $26
      (i32.sub
        (i32.const 23)
        (get_local $10)
      )
    )
    (set_local $27
      (i32.gt_s
        (get_local $3)
        (i32.const 0)
      )
    )
    (set_local $28
      (i32.eqz
        (get_local $10)
      )
    )
    (set_local $6
      (get_local $15)
    )
    (block $do-once18
      (block $__rjti$3
        (block $__rjti$2
          (loop $label$continue$L17
            (set_local $5
              (f64.load
                (i32.add
                  (get_local $16)
                  (i32.shl
                    (get_local $6)
                    (i32.const 3)
                  )
                )
              )
            )
            (if
              (tee_local $13
                (i32.gt_s
                  (get_local $6)
                  (i32.const 0)
                )
              )
              (block
                (set_local $8
                  (get_local $6)
                )
                (set_local $7
                  (i32.const 0)
                )
                (loop $while-in5
                  (i32.store
                    (i32.add
                      (get_local $14)
                      (i32.shl
                        (get_local $7)
                        (i32.const 2)
                      )
                    )
                    (call $f64-to-int
                      (f64.sub
                        (get_local $5)
                        (f64.mul
                          (tee_local $5
                            (f64.convert_s/i32
                              (call $f64-to-int
                                (f64.mul
                                  (get_local $5)
                                  (f64.const 5.9604644775390625e-08)
                                )
                              )
                            )
                          )
                          (f64.const 16777216)
                        )
                      )
                    )
                  )
                  (set_local $5
                    (f64.add
                      (get_local $5)
                      (f64.load
                        (i32.add
                          (get_local $16)
                          (i32.shl
                            (tee_local $9
                              (i32.add
                                (get_local $8)
                                (i32.const -1)
                              )
                            )
                            (i32.const 3)
                          )
                        )
                      )
                    )
                  )
                  (set_local $7
                    (i32.add
                      (get_local $7)
                      (i32.const 1)
                    )
                  )
                  (if
                    (i32.gt_s
                      (get_local $8)
                      (i32.const 1)
                    )
                    (block
                      (set_local $8
                        (get_local $9)
                      )
                      (br $while-in5)
                    )
                  )
                )
              )
            )
            (set_local $8
              (call $f64-to-int
                (tee_local $5
                  (f64.sub
                    (tee_local $5
                      (call $_scalbn
                        (get_local $5)
                        (get_local $10)
                      )
                    )
                    (f64.mul
                      (f64.floor
                        (f64.mul
                          (get_local $5)
                          (f64.const 0.125)
                        )
                      )
                      (f64.const 8)
                    )
                  )
                )
              )
            )
            (set_local $5
              (f64.sub
                (get_local $5)
                (f64.convert_s/i32
                  (get_local $8)
                )
              )
            )
            (block $__rjto$1
              (block $__rjti$1
                (block $__rjti$0
                  (if
                    (get_local $22)
                    (block
                      (set_local $7
                        (i32.shr_s
                          (tee_local $19
                            (i32.load
                              (tee_local $9
                                (i32.add
                                  (get_local $14)
                                  (i32.shl
                                    (i32.add
                                      (get_local $6)
                                      (i32.const -1)
                                    )
                                    (i32.const 2)
                                  )
                                )
                              )
                            )
                          )
                          (get_local $23)
                        )
                      )
                      (i32.store
                        (get_local $9)
                        (tee_local $9
                          (i32.sub
                            (get_local $19)
                            (i32.shl
                              (get_local $7)
                              (get_local $23)
                            )
                          )
                        )
                      )
                      (set_local $9
                        (i32.shr_s
                          (get_local $9)
                          (get_local $26)
                        )
                      )
                      (set_local $8
                        (i32.add
                          (get_local $7)
                          (get_local $8)
                        )
                      )
                      (br $__rjti$0)
                    )
                    (if
                      (get_local $28)
                      (block
                        (set_local $9
                          (i32.shr_s
                            (i32.load
                              (i32.add
                                (get_local $14)
                                (i32.shl
                                  (i32.add
                                    (get_local $6)
                                    (i32.const -1)
                                  )
                                  (i32.const 2)
                                )
                              )
                            )
                            (i32.const 23)
                          )
                        )
                        (br $__rjti$0)
                      )
                      (if
                        (f64.ge
                          (get_local $5)
                          (f64.const 0.5)
                        )
                        (block
                          (set_local $9
                            (i32.const 2)
                          )
                          (set_local $7
                            (get_local $8)
                          )
                          (br $__rjti$1)
                        )
                        (set_local $9
                          (i32.const 0)
                        )
                      )
                    )
                  )
                  (br $__rjto$1)
                )
                (if
                  (i32.gt_s
                    (get_local $9)
                    (i32.const 0)
                  )
                  (block
                    (set_local $7
                      (get_local $8)
                    )
                    (br $__rjti$1)
                  )
                )
                (br $__rjto$1)
              )
              (if
                (get_local $13)
                (block
                  (set_local $8
                    (i32.const 0)
                  )
                  (set_local $13
                    (i32.const 0)
                  )
                  (loop $while-in7
                    (set_local $19
                      (i32.load
                        (tee_local $24
                          (i32.add
                            (get_local $14)
                            (i32.shl
                              (get_local $13)
                              (i32.const 2)
                            )
                          )
                        )
                      )
                    )
                    (if
                      (get_local $8)
                      (i32.store
                        (get_local $24)
                        (i32.sub
                          (i32.const 16777215)
                          (get_local $19)
                        )
                      )
                      (set_local $8
                        (if i32
                          (get_local $19)
                          (block i32
                            (i32.store
                              (get_local $24)
                              (i32.sub
                                (i32.const 16777216)
                                (get_local $19)
                              )
                            )
                            (i32.const 1)
                          )
                          (i32.const 0)
                        )
                      )
                    )
                    (br_if $while-in7
                      (i32.ne
                        (tee_local $13
                          (i32.add
                            (get_local $13)
                            (i32.const 1)
                          )
                        )
                        (get_local $6)
                      )
                    )
                    (set_local $13
                      (get_local $8)
                    )
                  )
                )
                (set_local $13
                  (i32.const 0)
                )
              )
              (set_local $8
                (i32.add
                  (get_local $7)
                  (i32.const 1)
                )
              )
              (block $label$break$L42
                (if
                  (get_local $22)
                  (block $switch-default
                    (block $switch-case9
                      (block $switch-case
                        (br_table $switch-case $switch-case9 $switch-default
                          (i32.sub
                            (get_local $10)
                            (i32.const 1)
                          )
                        )
                      )
                      (i32.store
                        (tee_local $7
                          (i32.add
                            (get_local $14)
                            (i32.shl
                              (i32.add
                                (get_local $6)
                                (i32.const -1)
                              )
                              (i32.const 2)
                            )
                          )
                        )
                        (i32.and
                          (i32.load
                            (get_local $7)
                          )
                          (i32.const 8388607)
                        )
                      )
                      (br $label$break$L42)
                    )
                    (i32.store
                      (tee_local $7
                        (i32.add
                          (get_local $14)
                          (i32.shl
                            (i32.add
                              (get_local $6)
                              (i32.const -1)
                            )
                            (i32.const 2)
                          )
                        )
                      )
                      (i32.and
                        (i32.load
                          (get_local $7)
                        )
                        (i32.const 4194303)
                      )
                    )
                  )
                )
              )
              (if
                (i32.eq
                  (get_local $9)
                  (i32.const 2)
                )
                (block
                  (set_local $5
                    (f64.sub
                      (f64.const 1)
                      (get_local $5)
                    )
                  )
                  (set_local $9
                    (if i32
                      (get_local $13)
                      (block i32
                        (set_local $5
                          (f64.sub
                            (get_local $5)
                            (call $_scalbn
                              (f64.const 1)
                              (get_local $10)
                            )
                          )
                        )
                        (i32.const 2)
                      )
                      (i32.const 2)
                    )
                  )
                )
              )
            )
            (br_if $__rjti$3
              (f64.ne
                (get_local $5)
                (f64.const 0)
              )
            )
            (if
              (i32.gt_s
                (get_local $6)
                (get_local $15)
              )
              (block
                (set_local $13
                  (i32.const 0)
                )
                (set_local $7
                  (get_local $6)
                )
                (loop $while-in11
                  (set_local $13
                    (i32.or
                      (i32.load
                        (i32.add
                          (get_local $14)
                          (i32.shl
                            (tee_local $7
                              (i32.add
                                (get_local $7)
                                (i32.const -1)
                              )
                            )
                            (i32.const 2)
                          )
                        )
                      )
                      (get_local $13)
                    )
                  )
                  (br_if $while-in11
                    (i32.gt_s
                      (get_local $7)
                      (get_local $15)
                    )
                  )
                )
                (if
                  (get_local $13)
                  (block
                    (set_local $0
                      (get_local $10)
                    )
                    (br $__rjti$2)
                  )
                  (set_local $7
                    (i32.const 1)
                  )
                )
              )
              (set_local $7
                (i32.const 1)
              )
            )
            (loop $while-in13
              (set_local $8
                (i32.add
                  (get_local $7)
                  (i32.const 1)
                )
              )
              (if
                (i32.eqz
                  (i32.load
                    (i32.add
                      (get_local $14)
                      (i32.shl
                        (i32.sub
                          (get_local $15)
                          (get_local $7)
                        )
                        (i32.const 2)
                      )
                    )
                  )
                )
                (block
                  (set_local $7
                    (get_local $8)
                  )
                  (br $while-in13)
                )
              )
            )
            (set_local $8
              (i32.add
                (get_local $7)
                (get_local $6)
              )
            )
            (if
              (i32.le_s
                (get_local $7)
                (i32.const 0)
              )
              (block
                (set_local $6
                  (get_local $8)
                )
                (br $label$continue$L17)
              )
            )
            (loop $while-in15
              (f64.store
                (i32.add
                  (get_local $20)
                  (i32.shl
                    (tee_local $9
                      (i32.add
                        (get_local $6)
                        (get_local $3)
                      )
                    )
                    (i32.const 3)
                  )
                )
                (f64.convert_s/i32
                  (i32.load
                    (i32.add
                      (i32.shl
                        (i32.add
                          (tee_local $7
                            (i32.add
                              (get_local $6)
                              (i32.const 1)
                            )
                          )
                          (get_local $21)
                        )
                        (i32.const 2)
                      )
                      (i32.const 1976)
                    )
                  )
                )
              )
              (if
                (get_local $27)
                (block
                  (set_local $5
                    (f64.const 0)
                  )
                  (set_local $6
                    (i32.const 0)
                  )
                  (loop $while-in17
                    (set_local $5
                      (f64.add
                        (get_local $5)
                        (f64.mul
                          (f64.load
                            (i32.add
                              (get_local $0)
                              (i32.shl
                                (get_local $6)
                                (i32.const 3)
                              )
                            )
                          )
                          (f64.load
                            (i32.add
                              (get_local $20)
                              (i32.shl
                                (i32.sub
                                  (get_local $9)
                                  (get_local $6)
                                )
                                (i32.const 3)
                              )
                            )
                          )
                        )
                      )
                    )
                    (br_if $while-in17
                      (i32.ne
                        (tee_local $6
                          (i32.add
                            (get_local $6)
                            (i32.const 1)
                          )
                        )
                        (get_local $3)
                      )
                    )
                  )
                )
                (set_local $5
                  (f64.const 0)
                )
              )
              (f64.store
                (i32.add
                  (get_local $16)
                  (i32.shl
                    (get_local $7)
                    (i32.const 3)
                  )
                )
                (get_local $5)
              )
              (if
                (i32.lt_s
                  (get_local $7)
                  (get_local $8)
                )
                (block
                  (set_local $6
                    (get_local $7)
                  )
                  (br $while-in15)
                )
                (block
                  (set_local $6
                    (get_local $8)
                  )
                  (br $label$continue$L17)
                )
              )
            )
          )
        )
        (loop $while-in21
          (set_local $0
            (i32.add
              (get_local $0)
              (i32.const -24)
            )
          )
          (br_if $while-in21
            (i32.eqz
              (i32.load
                (i32.add
                  (get_local $14)
                  (i32.shl
                    (tee_local $6
                      (i32.add
                        (get_local $6)
                        (i32.const -1)
                      )
                    )
                    (i32.const 2)
                  )
                )
              )
            )
          )
          (set_local $10
            (get_local $0)
          )
          (set_local $0
            (get_local $6)
          )
        )
        (br $do-once18)
      )
      (if
        (f64.ge
          (tee_local $5
            (call $_scalbn
              (get_local $5)
              (i32.sub
                (i32.const 0)
                (get_local $10)
              )
            )
          )
          (f64.const 16777216)
        )
        (block
          (i32.store
            (i32.add
              (get_local $14)
              (i32.shl
                (get_local $6)
                (i32.const 2)
              )
            )
            (call $f64-to-int
              (f64.sub
                (get_local $5)
                (f64.mul
                  (f64.convert_s/i32
                    (tee_local $3
                      (call $f64-to-int
                        (f64.mul
                          (get_local $5)
                          (f64.const 5.9604644775390625e-08)
                        )
                      )
                    )
                  )
                  (f64.const 16777216)
                )
              )
            )
          )
          (i32.store
            (i32.add
              (get_local $14)
              (i32.shl
                (tee_local $0
                  (i32.add
                    (get_local $6)
                    (i32.const 1)
                  )
                )
                (i32.const 2)
              )
            )
            (get_local $3)
          )
          (set_local $10
            (i32.add
              (get_local $25)
              (get_local $2)
            )
          )
        )
        (block
          (i32.store
            (i32.add
              (get_local $14)
              (i32.shl
                (get_local $6)
                (i32.const 2)
              )
            )
            (call $f64-to-int
              (get_local $5)
            )
          )
          (set_local $0
            (get_local $6)
          )
        )
      )
    )
    (if
      (tee_local $7
        (i32.gt_s
          (get_local $0)
          (i32.const -1)
        )
      )
      (block
        (set_local $5
          (call $_scalbn
            (f64.const 1)
            (get_local $10)
          )
        )
        (set_local $2
          (get_local $0)
        )
        (loop $while-in23
          (f64.store
            (i32.add
              (get_local $16)
              (i32.shl
                (get_local $2)
                (i32.const 3)
              )
            )
            (f64.mul
              (get_local $5)
              (f64.convert_s/i32
                (i32.load
                  (i32.add
                    (get_local $14)
                    (i32.shl
                      (get_local $2)
                      (i32.const 2)
                    )
                  )
                )
              )
            )
          )
          (set_local $5
            (f64.mul
              (get_local $5)
              (f64.const 5.9604644775390625e-08)
            )
          )
          (set_local $3
            (i32.add
              (get_local $2)
              (i32.const -1)
            )
          )
          (if
            (i32.gt_s
              (get_local $2)
              (i32.const 0)
            )
            (block
              (set_local $2
                (get_local $3)
              )
              (br $while-in23)
            )
          )
        )
        (if
          (get_local $7)
          (block
            (set_local $2
              (get_local $0)
            )
            (loop $while-in25
              (set_local $10
                (i32.sub
                  (get_local $0)
                  (get_local $2)
                )
              )
              (set_local $3
                (i32.const 0)
              )
              (set_local $5
                (f64.const 0)
              )
              (loop $while-in27
                (set_local $5
                  (f64.add
                    (get_local $5)
                    (f64.mul
                      (f64.load
                        (i32.add
                          (i32.shl
                            (get_local $3)
                            (i32.const 3)
                          )
                          (i32.const 1360)
                        )
                      )
                      (f64.load
                        (i32.add
                          (get_local $16)
                          (i32.shl
                            (i32.add
                              (get_local $3)
                              (get_local $2)
                            )
                            (i32.const 3)
                          )
                        )
                      )
                    )
                  )
                )
                (set_local $6
                  (i32.add
                    (get_local $3)
                    (i32.const 1)
                  )
                )
                (if
                  (i32.eqz
                    (i32.or
                      (i32.ge_s
                        (get_local $3)
                        (get_local $15)
                      )
                      (i32.ge_s
                        (get_local $3)
                        (get_local $10)
                      )
                    )
                  )
                  (block
                    (set_local $3
                      (get_local $6)
                    )
                    (br $while-in27)
                  )
                )
              )
              (f64.store
                (i32.add
                  (get_local $12)
                  (i32.shl
                    (get_local $10)
                    (i32.const 3)
                  )
                )
                (get_local $5)
              )
              (set_local $3
                (i32.add
                  (get_local $2)
                  (i32.const -1)
                )
              )
              (if
                (i32.gt_s
                  (get_local $2)
                  (i32.const 0)
                )
                (block
                  (set_local $2
                    (get_local $3)
                  )
                  (br $while-in25)
                )
              )
            )
          )
        )
      )
    )
    (block $switch-default46
      (block $switch-case45
        (block $switch-case33
          (block $switch-case32
            (br_table $switch-case32 $switch-case33 $switch-case33 $switch-case45 $switch-default46
              (i32.sub
                (get_local $4)
                (i32.const 0)
              )
            )
          )
          (if
            (get_local $7)
            (block
              (set_local $5
                (f64.const 0)
              )
              (loop $while-in31
                (set_local $5
                  (f64.add
                    (get_local $5)
                    (f64.load
                      (i32.add
                        (get_local $12)
                        (i32.shl
                          (get_local $0)
                          (i32.const 3)
                        )
                      )
                    )
                  )
                )
                (set_local $2
                  (i32.add
                    (get_local $0)
                    (i32.const -1)
                  )
                )
                (if
                  (i32.gt_s
                    (get_local $0)
                    (i32.const 0)
                  )
                  (block
                    (set_local $0
                      (get_local $2)
                    )
                    (br $while-in31)
                  )
                )
              )
            )
            (set_local $5
              (f64.const 0)
            )
          )
          (set_local $11
            (f64.neg
              (get_local $5)
            )
          )
          (f64.store
            (get_local $1)
            (if f64
              (get_local $9)
              (get_local $11)
              (get_local $5)
            )
          )
          (br $switch-default46)
        )
        (if
          (get_local $7)
          (block
            (set_local $5
              (f64.const 0)
            )
            (set_local $2
              (get_local $0)
            )
            (loop $while-in35
              (set_local $5
                (f64.add
                  (get_local $5)
                  (f64.load
                    (i32.add
                      (get_local $12)
                      (i32.shl
                        (get_local $2)
                        (i32.const 3)
                      )
                    )
                  )
                )
              )
              (set_local $3
                (i32.add
                  (get_local $2)
                  (i32.const -1)
                )
              )
              (if
                (i32.gt_s
                  (get_local $2)
                  (i32.const 0)
                )
                (block
                  (set_local $2
                    (get_local $3)
                  )
                  (br $while-in35)
                )
              )
            )
          )
          (set_local $5
            (f64.const 0)
          )
        )
        (set_local $11
          (f64.neg
            (get_local $5)
          )
        )
        (f64.store
          (get_local $1)
          (if f64
            (tee_local $4
              (i32.eqz
                (get_local $9)
              )
            )
            (get_local $5)
            (get_local $11)
          )
        )
        (set_local $5
          (f64.sub
            (f64.load
              (get_local $12)
            )
            (get_local $5)
          )
        )
        (if
          (i32.ge_s
            (get_local $0)
            (i32.const 1)
          )
          (block
            (set_local $2
              (i32.const 1)
            )
            (loop $while-in37
              (set_local $5
                (f64.add
                  (get_local $5)
                  (f64.load
                    (i32.add
                      (get_local $12)
                      (i32.shl
                        (get_local $2)
                        (i32.const 3)
                      )
                    )
                  )
                )
              )
              (set_local $3
                (i32.add
                  (get_local $2)
                  (i32.const 1)
                )
              )
              (if
                (i32.ne
                  (get_local $2)
                  (get_local $0)
                )
                (block
                  (set_local $2
                    (get_local $3)
                  )
                  (br $while-in37)
                )
              )
            )
          )
        )
        (set_local $11
          (f64.neg
            (get_local $5)
          )
        )
        (f64.store offset=8
          (get_local $1)
          (if f64
            (get_local $4)
            (get_local $5)
            (get_local $11)
          )
        )
        (br $switch-default46)
      )
      (if
        (i32.gt_s
          (get_local $0)
          (i32.const 0)
        )
        (block
          (set_local $2
            (get_local $0)
          )
          (set_local $5
            (f64.load
              (i32.add
                (get_local $12)
                (i32.shl
                  (get_local $0)
                  (i32.const 3)
                )
              )
            )
          )
          (loop $while-in40
            (set_local $11
              (f64.add
                (tee_local $17
                  (f64.load
                    (tee_local $4
                      (i32.add
                        (get_local $12)
                        (i32.shl
                          (tee_local $3
                            (i32.add
                              (get_local $2)
                              (i32.const -1)
                            )
                          )
                          (i32.const 3)
                        )
                      )
                    )
                  )
                )
                (get_local $5)
              )
            )
            (f64.store
              (i32.add
                (get_local $12)
                (i32.shl
                  (get_local $2)
                  (i32.const 3)
                )
              )
              (f64.add
                (get_local $5)
                (f64.sub
                  (get_local $17)
                  (get_local $11)
                )
              )
            )
            (f64.store
              (get_local $4)
              (get_local $11)
            )
            (if
              (i32.gt_s
                (get_local $2)
                (i32.const 1)
              )
              (block
                (set_local $2
                  (get_local $3)
                )
                (set_local $5
                  (get_local $11)
                )
                (br $while-in40)
              )
            )
          )
          (if
            (tee_local $4
              (i32.gt_s
                (get_local $0)
                (i32.const 1)
              )
            )
            (block
              (set_local $2
                (get_local $0)
              )
              (set_local $5
                (f64.load
                  (i32.add
                    (get_local $12)
                    (i32.shl
                      (get_local $0)
                      (i32.const 3)
                    )
                  )
                )
              )
              (loop $while-in42
                (set_local $11
                  (f64.add
                    (tee_local $17
                      (f64.load
                        (tee_local $6
                          (i32.add
                            (get_local $12)
                            (i32.shl
                              (tee_local $3
                                (i32.add
                                  (get_local $2)
                                  (i32.const -1)
                                )
                              )
                              (i32.const 3)
                            )
                          )
                        )
                      )
                    )
                    (get_local $5)
                  )
                )
                (f64.store
                  (i32.add
                    (get_local $12)
                    (i32.shl
                      (get_local $2)
                      (i32.const 3)
                    )
                  )
                  (f64.add
                    (get_local $5)
                    (f64.sub
                      (get_local $17)
                      (get_local $11)
                    )
                  )
                )
                (f64.store
                  (get_local $6)
                  (get_local $11)
                )
                (if
                  (i32.gt_s
                    (get_local $3)
                    (i32.const 1)
                  )
                  (block
                    (set_local $2
                      (get_local $3)
                    )
                    (set_local $5
                      (get_local $11)
                    )
                    (br $while-in42)
                  )
                )
              )
              (if
                (get_local $4)
                (block
                  (set_local $5
                    (f64.const 0)
                  )
                  (loop $while-in44
                    (set_local $5
                      (f64.add
                        (get_local $5)
                        (f64.load
                          (i32.add
                            (get_local $12)
                            (i32.shl
                              (get_local $0)
                              (i32.const 3)
                            )
                          )
                        )
                      )
                    )
                    (br_if $while-in44
                      (i32.gt_s
                        (tee_local $0
                          (i32.add
                            (get_local $0)
                            (i32.const -1)
                          )
                        )
                        (i32.const 1)
                      )
                    )
                  )
                )
                (set_local $5
                  (f64.const 0)
                )
              )
            )
            (set_local $5
              (f64.const 0)
            )
          )
        )
        (set_local $5
          (f64.const 0)
        )
      )
      (set_local $11
        (f64.load
          (get_local $12)
        )
      )
      (set_local $17
        (f64.load offset=8
          (get_local $12)
        )
      )
      (if
        (get_local $9)
        (block
          (f64.store
            (get_local $1)
            (f64.neg
              (get_local $11)
            )
          )
          (f64.store offset=8
            (get_local $1)
            (f64.neg
              (get_local $17)
            )
          )
          (f64.store offset=16
            (get_local $1)
            (f64.neg
              (get_local $5)
            )
          )
        )
        (block
          (f64.store
            (get_local $1)
            (get_local $11)
          )
          (f64.store offset=8
            (get_local $1)
            (get_local $17)
          )
          (f64.store offset=16
            (get_local $1)
            (get_local $5)
          )
        )
      )
    )
    (set_global $STACKTOP
      (get_local $18)
    )
    (i32.and
      (get_local $8)
      (i32.const 7)
    )
  )
  (func $___sin (param $0 f64) (param $1 f64) (param $2 i32) (result f64)
    (local $3 f64)
    (local $4 f64)
    (local $5 f64)
    (set_local $5
      (f64.add
        (f64.mul
          (f64.mul
            (tee_local $3
              (f64.mul
                (get_local $0)
                (get_local $0)
              )
            )
            (f64.mul
              (get_local $3)
              (get_local $3)
            )
          )
          (f64.add
            (f64.mul
              (get_local $3)
              (f64.const 1.58969099521155e-10)
            )
            (f64.const -2.5050760253406863e-08)
          )
        )
        (f64.add
          (f64.mul
            (get_local $3)
            (f64.add
              (f64.mul
                (get_local $3)
                (f64.const 2.7557313707070068e-06)
              )
              (f64.const -1.984126982985795e-04)
            )
          )
          (f64.const 0.00833333333332249)
        )
      )
    )
    (set_local $4
      (f64.mul
        (get_local $3)
        (get_local $0)
      )
    )
    (tee_local $0
      (if f64
        (get_local $2)
        (f64.sub
          (get_local $0)
          (f64.add
            (f64.mul
              (get_local $4)
              (f64.const 0.16666666666666632)
            )
            (f64.sub
              (f64.mul
                (get_local $3)
                (f64.sub
                  (f64.mul
                    (get_local $1)
                    (f64.const 0.5)
                  )
                  (f64.mul
                    (get_local $4)
                    (get_local $5)
                  )
                )
              )
              (get_local $1)
            )
          )
        )
        (f64.add
          (f64.mul
            (get_local $4)
            (f64.add
              (f64.mul
                (get_local $3)
                (get_local $5)
              )
              (f64.const -0.16666666666666632)
            )
          )
          (get_local $0)
        )
      )
    )
  )
  (func $_fflush (param $0 i32) (result i32)
    (local $1 i32)
    (local $2 i32)
    (block $do-once
      (if
        (get_local $0)
        (block
          (if
            (i32.le_s
              (i32.load offset=76
                (get_local $0)
              )
              (i32.const -1)
            )
            (block
              (set_local $0
                (call $___fflush_unlocked
                  (get_local $0)
                )
              )
              (br $do-once)
            )
          )
          (set_local $2
            (i32.eqz
              (call $___lockfile
                (get_local $0)
              )
            )
          )
          (set_local $1
            (call $___fflush_unlocked
              (get_local $0)
            )
          )
          (set_local $0
            (if i32
              (get_local $2)
              (get_local $1)
              (block i32
                (call $___unlockfile
                  (get_local $0)
                )
                (get_local $1)
              )
            )
          )
        )
        (block
          (set_local $0
            (if i32
              (i32.load
                (i32.const 1844)
              )
              (call $_fflush
                (i32.load
                  (i32.const 1844)
                )
              )
              (i32.const 0)
            )
          )
          (call $___lock
            (i32.const 5524)
          )
          (if
            (tee_local $1
              (i32.load
                (i32.const 5520)
              )
            )
            (loop $while-in
              (set_local $2
                (if i32
                  (i32.gt_s
                    (i32.load offset=76
                      (get_local $1)
                    )
                    (i32.const -1)
                  )
                  (call $___lockfile
                    (get_local $1)
                  )
                  (i32.const 0)
                )
              )
              (if
                (i32.gt_u
                  (i32.load offset=20
                    (get_local $1)
                  )
                  (i32.load offset=28
                    (get_local $1)
                  )
                )
                (set_local $0
                  (i32.or
                    (call $___fflush_unlocked
                      (get_local $1)
                    )
                    (get_local $0)
                  )
                )
              )
              (if
                (get_local $2)
                (call $___unlockfile
                  (get_local $1)
                )
              )
              (br_if $while-in
                (tee_local $1
                  (i32.load offset=56
                    (get_local $1)
                  )
                )
              )
            )
          )
          (call $___unlock
            (i32.const 5524)
          )
        )
      )
    )
    (get_local $0)
  )
  (func $___fflush_unlocked (param $0 i32) (result i32)
    (local $1 i32)
    (local $2 i32)
    (local $3 i32)
    (local $4 i32)
    (local $5 i32)
    (local $6 i32)
    (tee_local $0
      (block $__rjto$0 i32
        (block $__rjti$0
          (br_if $__rjti$0
            (i32.le_u
              (i32.load
                (tee_local $1
                  (i32.add
                    (get_local $0)
                    (i32.const 20)
                  )
                )
              )
              (i32.load
                (tee_local $2
                  (i32.add
                    (get_local $0)
                    (i32.const 28)
                  )
                )
              )
            )
          )
          (drop
            (call_indirect $FUNCSIG$iiii
              (get_local $0)
              (i32.const 0)
              (i32.const 0)
              (i32.add
                (i32.and
                  (i32.load offset=36
                    (get_local $0)
                  )
                  (i32.const 7)
                )
                (i32.const 0)
              )
            )
          )
          (br_if $__rjti$0
            (i32.load
              (get_local $1)
            )
          )
          (br $__rjto$0
            (i32.const -1)
          )
        )
        (if
          (i32.lt_u
            (tee_local $4
              (i32.load
                (tee_local $3
                  (i32.add
                    (get_local $0)
                    (i32.const 4)
                  )
                )
              )
            )
            (tee_local $6
              (i32.load
                (tee_local $5
                  (i32.add
                    (get_local $0)
                    (i32.const 8)
                  )
                )
              )
            )
          )
          (drop
            (call_indirect $FUNCSIG$iiii
              (get_local $0)
              (i32.sub
                (get_local $4)
                (get_local $6)
              )
              (i32.const 1)
              (i32.add
                (i32.and
                  (i32.load offset=40
                    (get_local $0)
                  )
                  (i32.const 7)
                )
                (i32.const 0)
              )
            )
          )
        )
        (i32.store offset=16
          (get_local $0)
          (i32.const 0)
        )
        (i32.store
          (get_local $2)
          (i32.const 0)
        )
        (i32.store
          (get_local $1)
          (i32.const 0)
        )
        (i32.store
          (get_local $5)
          (i32.const 0)
        )
        (i32.store
          (get_local $3)
          (i32.const 0)
        )
        (i32.const 0)
      )
    )
  )
  (func $_fprintf (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
    (local $3 i32)
    (local $4 i32)
    (set_local $3
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 16)
      )
    )
    (i32.store
      (tee_local $4
        (get_local $3)
      )
      (get_local $2)
    )
    (set_local $0
      (call $_vfprintf
        (get_local $0)
        (get_local $1)
        (get_local $4)
      )
    )
    (set_global $STACKTOP
      (get_local $3)
    )
    (get_local $0)
  )
  (func $_malloc (param $0 i32) (result i32)
    (local $1 i32)
    (local $2 i32)
    (local $3 i32)
    (local $4 i32)
    (local $5 i32)
    (local $6 i32)
    (local $7 i32)
    (local $8 i32)
    (local $9 i32)
    (local $10 i32)
    (local $11 i32)
    (local $12 i32)
    (local $13 i32)
    (local $14 i32)
    (local $15 i32)
    (local $16 i32)
    (local $17 i32)
    (local $18 i32)
    (local $19 i32)
    (local $20 i32)
    (set_local $13
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 16)
      )
    )
    (set_local $15
      (get_local $13)
    )
    (block $do-once
      (if
        (i32.lt_u
          (get_local $0)
          (i32.const 245)
        )
        (block
          (set_local $2
            (i32.and
              (i32.add
                (get_local $0)
                (i32.const 11)
              )
              (i32.const -8)
            )
          )
          (if
            (i32.and
              (tee_local $1
                (i32.shr_u
                  (tee_local $7
                    (i32.load
                      (i32.const 5544)
                    )
                  )
                  (tee_local $0
                    (i32.shr_u
                      (if i32
                        (i32.lt_u
                          (get_local $0)
                          (i32.const 11)
                        )
                        (tee_local $2
                          (i32.const 16)
                        )
                        (get_local $2)
                      )
                      (i32.const 3)
                    )
                  )
                )
              )
              (i32.const 3)
            )
            (block
              (set_local $0
                (i32.load
                  (tee_local $6
                    (i32.add
                      (tee_local $1
                        (i32.load
                          (tee_local $5
                            (i32.add
                              (tee_local $3
                                (i32.add
                                  (i32.shl
                                    (i32.shl
                                      (tee_local $2
                                        (i32.add
                                          (i32.xor
                                            (i32.and
                                              (get_local $1)
                                              (i32.const 1)
                                            )
                                            (i32.const 1)
                                          )
                                          (get_local $0)
                                        )
                                      )
                                      (i32.const 1)
                                    )
                                    (i32.const 2)
                                  )
                                  (i32.const 5584)
                                )
                              )
                              (i32.const 8)
                            )
                          )
                        )
                      )
                      (i32.const 8)
                    )
                  )
                )
              )
              (if
                (i32.eq
                  (get_local $3)
                  (get_local $0)
                )
                (i32.store
                  (i32.const 5544)
                  (i32.and
                    (get_local $7)
                    (i32.xor
                      (i32.shl
                        (i32.const 1)
                        (get_local $2)
                      )
                      (i32.const -1)
                    )
                  )
                )
                (block
                  (if
                    (i32.lt_u
                      (get_local $0)
                      (i32.load
                        (i32.const 5560)
                      )
                    )
                    (call $_abort)
                  )
                  (if
                    (i32.eq
                      (i32.load
                        (tee_local $4
                          (i32.add
                            (get_local $0)
                            (i32.const 12)
                          )
                        )
                      )
                      (get_local $1)
                    )
                    (block
                      (i32.store
                        (get_local $4)
                        (get_local $3)
                      )
                      (i32.store
                        (get_local $5)
                        (get_local $0)
                      )
                    )
                    (call $_abort)
                  )
                )
              )
              (i32.store offset=4
                (get_local $1)
                (i32.or
                  (tee_local $0
                    (i32.shl
                      (get_local $2)
                      (i32.const 3)
                    )
                  )
                  (i32.const 3)
                )
              )
              (i32.store
                (tee_local $0
                  (i32.add
                    (i32.add
                      (get_local $1)
                      (get_local $0)
                    )
                    (i32.const 4)
                  )
                )
                (i32.or
                  (i32.load
                    (get_local $0)
                  )
                  (i32.const 1)
                )
              )
              (set_global $STACKTOP
                (get_local $13)
              )
              (return
                (get_local $6)
              )
            )
          )
          (if
            (i32.gt_u
              (get_local $2)
              (tee_local $16
                (i32.load
                  (i32.const 5552)
                )
              )
            )
            (block
              (if
                (get_local $1)
                (block
                  (set_local $0
                    (i32.and
                      (i32.shr_u
                        (tee_local $1
                          (i32.add
                            (i32.and
                              (tee_local $0
                                (i32.and
                                  (i32.shl
                                    (get_local $1)
                                    (get_local $0)
                                  )
                                  (i32.or
                                    (tee_local $0
                                      (i32.shl
                                        (i32.const 2)
                                        (get_local $0)
                                      )
                                    )
                                    (i32.sub
                                      (i32.const 0)
                                      (get_local $0)
                                    )
                                  )
                                )
                              )
                              (i32.sub
                                (i32.const 0)
                                (get_local $0)
                              )
                            )
                            (i32.const -1)
                          )
                        )
                        (i32.const 12)
                      )
                      (i32.const 16)
                    )
                  )
                  (set_local $0
                    (i32.load
                      (tee_local $10
                        (i32.add
                          (tee_local $1
                            (i32.load
                              (tee_local $8
                                (i32.add
                                  (tee_local $4
                                    (i32.add
                                      (i32.shl
                                        (i32.shl
                                          (tee_local $5
                                            (i32.add
                                              (i32.or
                                                (i32.or
                                                  (i32.or
                                                    (i32.or
                                                      (tee_local $5
                                                        (i32.and
                                                          (i32.shr_u
                                                            (tee_local $1
                                                              (i32.shr_u
                                                                (get_local $1)
                                                                (get_local $0)
                                                              )
                                                            )
                                                            (i32.const 5)
                                                          )
                                                          (i32.const 8)
                                                        )
                                                      )
                                                      (get_local $0)
                                                    )
                                                    (tee_local $1
                                                      (i32.and
                                                        (i32.shr_u
                                                          (tee_local $0
                                                            (i32.shr_u
                                                              (get_local $1)
                                                              (get_local $5)
                                                            )
                                                          )
                                                          (i32.const 2)
                                                        )
                                                        (i32.const 4)
                                                      )
                                                    )
                                                  )
                                                  (tee_local $1
                                                    (i32.and
                                                      (i32.shr_u
                                                        (tee_local $0
                                                          (i32.shr_u
                                                            (get_local $0)
                                                            (get_local $1)
                                                          )
                                                        )
                                                        (i32.const 1)
                                                      )
                                                      (i32.const 2)
                                                    )
                                                  )
                                                )
                                                (tee_local $1
                                                  (i32.and
                                                    (i32.shr_u
                                                      (tee_local $0
                                                        (i32.shr_u
                                                          (get_local $0)
                                                          (get_local $1)
                                                        )
                                                      )
                                                      (i32.const 1)
                                                    )
                                                    (i32.const 1)
                                                  )
                                                )
                                              )
                                              (i32.shr_u
                                                (get_local $0)
                                                (get_local $1)
                                              )
                                            )
                                          )
                                          (i32.const 1)
                                        )
                                        (i32.const 2)
                                      )
                                      (i32.const 5584)
                                    )
                                  )
                                  (i32.const 8)
                                )
                              )
                            )
                          )
                          (i32.const 8)
                        )
                      )
                    )
                  )
                  (if
                    (i32.eq
                      (get_local $4)
                      (get_local $0)
                    )
                    (i32.store
                      (i32.const 5544)
                      (tee_local $3
                        (i32.and
                          (get_local $7)
                          (i32.xor
                            (i32.shl
                              (i32.const 1)
                              (get_local $5)
                            )
                            (i32.const -1)
                          )
                        )
                      )
                    )
                    (block
                      (if
                        (i32.lt_u
                          (get_local $0)
                          (i32.load
                            (i32.const 5560)
                          )
                        )
                        (call $_abort)
                      )
                      (if
                        (i32.eq
                          (i32.load
                            (tee_local $12
                              (i32.add
                                (get_local $0)
                                (i32.const 12)
                              )
                            )
                          )
                          (get_local $1)
                        )
                        (block
                          (i32.store
                            (get_local $12)
                            (get_local $4)
                          )
                          (i32.store
                            (get_local $8)
                            (get_local $0)
                          )
                          (set_local $3
                            (get_local $7)
                          )
                        )
                        (call $_abort)
                      )
                    )
                  )
                  (i32.store offset=4
                    (get_local $1)
                    (i32.or
                      (get_local $2)
                      (i32.const 3)
                    )
                  )
                  (i32.store offset=4
                    (tee_local $8
                      (i32.add
                        (get_local $1)
                        (get_local $2)
                      )
                    )
                    (i32.or
                      (tee_local $4
                        (i32.sub
                          (i32.shl
                            (get_local $5)
                            (i32.const 3)
                          )
                          (get_local $2)
                        )
                      )
                      (i32.const 1)
                    )
                  )
                  (i32.store
                    (i32.add
                      (get_local $8)
                      (get_local $4)
                    )
                    (get_local $4)
                  )
                  (if
                    (get_local $16)
                    (block
                      (set_local $5
                        (i32.load
                          (i32.const 5564)
                        )
                      )
                      (set_local $0
                        (i32.add
                          (i32.shl
                            (i32.shl
                              (tee_local $1
                                (i32.shr_u
                                  (get_local $16)
                                  (i32.const 3)
                                )
                              )
                              (i32.const 1)
                            )
                            (i32.const 2)
                          )
                          (i32.const 5584)
                        )
                      )
                      (if
                        (i32.and
                          (get_local $3)
                          (tee_local $1
                            (i32.shl
                              (i32.const 1)
                              (get_local $1)
                            )
                          )
                        )
                        (if
                          (i32.lt_u
                            (tee_local $2
                              (i32.load
                                (tee_local $1
                                  (i32.add
                                    (get_local $0)
                                    (i32.const 8)
                                  )
                                )
                              )
                            )
                            (i32.load
                              (i32.const 5560)
                            )
                          )
                          (call $_abort)
                          (block
                            (set_local $6
                              (get_local $2)
                            )
                            (set_local $11
                              (get_local $1)
                            )
                          )
                        )
                        (block
                          (i32.store
                            (i32.const 5544)
                            (i32.or
                              (get_local $3)
                              (get_local $1)
                            )
                          )
                          (set_local $6
                            (get_local $0)
                          )
                          (set_local $11
                            (i32.add
                              (get_local $0)
                              (i32.const 8)
                            )
                          )
                        )
                      )
                      (i32.store
                        (get_local $11)
                        (get_local $5)
                      )
                      (i32.store offset=12
                        (get_local $6)
                        (get_local $5)
                      )
                      (i32.store offset=8
                        (get_local $5)
                        (get_local $6)
                      )
                      (i32.store offset=12
                        (get_local $5)
                        (get_local $0)
                      )
                    )
                  )
                  (i32.store
                    (i32.const 5552)
                    (get_local $4)
                  )
                  (i32.store
                    (i32.const 5564)
                    (get_local $8)
                  )
                  (set_global $STACKTOP
                    (get_local $13)
                  )
                  (return
                    (get_local $10)
                  )
                )
              )
              (if
                (tee_local $11
                  (i32.load
                    (i32.const 5548)
                  )
                )
                (block
                  (set_local $0
                    (i32.and
                      (i32.shr_u
                        (tee_local $1
                          (i32.add
                            (i32.and
                              (get_local $11)
                              (i32.sub
                                (i32.const 0)
                                (get_local $11)
                              )
                            )
                            (i32.const -1)
                          )
                        )
                        (i32.const 12)
                      )
                      (i32.const 16)
                    )
                  )
                  (set_local $8
                    (tee_local $0
                      (i32.load
                        (i32.add
                          (i32.shl
                            (i32.add
                              (i32.or
                                (i32.or
                                  (i32.or
                                    (i32.or
                                      (tee_local $3
                                        (i32.and
                                          (i32.shr_u
                                            (tee_local $1
                                              (i32.shr_u
                                                (get_local $1)
                                                (get_local $0)
                                              )
                                            )
                                            (i32.const 5)
                                          )
                                          (i32.const 8)
                                        )
                                      )
                                      (get_local $0)
                                    )
                                    (tee_local $1
                                      (i32.and
                                        (i32.shr_u
                                          (tee_local $0
                                            (i32.shr_u
                                              (get_local $1)
                                              (get_local $3)
                                            )
                                          )
                                          (i32.const 2)
                                        )
                                        (i32.const 4)
                                      )
                                    )
                                  )
                                  (tee_local $1
                                    (i32.and
                                      (i32.shr_u
                                        (tee_local $0
                                          (i32.shr_u
                                            (get_local $0)
                                            (get_local $1)
                                          )
                                        )
                                        (i32.const 1)
                                      )
                                      (i32.const 2)
                                    )
                                  )
                                )
                                (tee_local $1
                                  (i32.and
                                    (i32.shr_u
                                      (tee_local $0
                                        (i32.shr_u
                                          (get_local $0)
                                          (get_local $1)
                                        )
                                      )
                                      (i32.const 1)
                                    )
                                    (i32.const 1)
                                  )
                                )
                              )
                              (i32.shr_u
                                (get_local $0)
                                (get_local $1)
                              )
                            )
                            (i32.const 2)
                          )
                          (i32.const 5848)
                        )
                      )
                    )
                  )
                  (set_local $3
                    (get_local $0)
                  )
                  (set_local $6
                    (i32.sub
                      (i32.and
                        (i32.load offset=4
                          (get_local $0)
                        )
                        (i32.const -8)
                      )
                      (get_local $2)
                    )
                  )
                  (loop $while-in
                    (block $while-out
                      (if
                        (i32.eqz
                          (tee_local $0
                            (i32.load offset=16
                              (get_local $8)
                            )
                          )
                        )
                        (br_if $while-out
                          (i32.eqz
                            (tee_local $0
                              (i32.load offset=20
                                (get_local $8)
                              )
                            )
                          )
                        )
                      )
                      (if
                        (i32.eqz
                          (tee_local $10
                            (i32.lt_u
                              (tee_local $1
                                (i32.sub
                                  (i32.and
                                    (i32.load offset=4
                                      (get_local $0)
                                    )
                                    (i32.const -8)
                                  )
                                  (get_local $2)
                                )
                              )
                              (get_local $6)
                            )
                          )
                        )
                        (set_local $1
                          (get_local $6)
                        )
                      )
                      (set_local $8
                        (get_local $0)
                      )
                      (if
                        (get_local $10)
                        (set_local $3
                          (get_local $0)
                        )
                      )
                      (set_local $6
                        (get_local $1)
                      )
                      (br $while-in)
                    )
                  )
                  (if
                    (i32.lt_u
                      (get_local $3)
                      (tee_local $15
                        (i32.load
                          (i32.const 5560)
                        )
                      )
                    )
                    (call $_abort)
                  )
                  (if
                    (i32.ge_u
                      (get_local $3)
                      (tee_local $9
                        (i32.add
                          (get_local $3)
                          (get_local $2)
                        )
                      )
                    )
                    (call $_abort)
                  )
                  (set_local $12
                    (i32.load offset=24
                      (get_local $3)
                    )
                  )
                  (block $do-once4
                    (if
                      (i32.eq
                        (tee_local $0
                          (i32.load offset=12
                            (get_local $3)
                          )
                        )
                        (get_local $3)
                      )
                      (block
                        (if
                          (i32.eqz
                            (tee_local $0
                              (i32.load
                                (tee_local $1
                                  (i32.add
                                    (get_local $3)
                                    (i32.const 20)
                                  )
                                )
                              )
                            )
                          )
                          (if
                            (i32.eqz
                              (tee_local $0
                                (i32.load
                                  (tee_local $1
                                    (i32.add
                                      (get_local $3)
                                      (i32.const 16)
                                    )
                                  )
                                )
                              )
                            )
                            (block
                              (set_local $5
                                (i32.const 0)
                              )
                              (br $do-once4)
                            )
                          )
                        )
                        (loop $while-in7
                          (if
                            (tee_local $10
                              (i32.load
                                (tee_local $8
                                  (i32.add
                                    (get_local $0)
                                    (i32.const 20)
                                  )
                                )
                              )
                            )
                            (block
                              (set_local $0
                                (get_local $10)
                              )
                              (set_local $1
                                (get_local $8)
                              )
                              (br $while-in7)
                            )
                          )
                          (if
                            (tee_local $10
                              (i32.load
                                (tee_local $8
                                  (i32.add
                                    (get_local $0)
                                    (i32.const 16)
                                  )
                                )
                              )
                            )
                            (block
                              (set_local $0
                                (get_local $10)
                              )
                              (set_local $1
                                (get_local $8)
                              )
                              (br $while-in7)
                            )
                          )
                        )
                        (if
                          (i32.lt_u
                            (get_local $1)
                            (get_local $15)
                          )
                          (call $_abort)
                          (block
                            (i32.store
                              (get_local $1)
                              (i32.const 0)
                            )
                            (set_local $5
                              (get_local $0)
                            )
                          )
                        )
                      )
                      (block
                        (if
                          (i32.lt_u
                            (tee_local $1
                              (i32.load offset=8
                                (get_local $3)
                              )
                            )
                            (get_local $15)
                          )
                          (call $_abort)
                        )
                        (if
                          (i32.ne
                            (i32.load
                              (tee_local $8
                                (i32.add
                                  (get_local $1)
                                  (i32.const 12)
                                )
                              )
                            )
                            (get_local $3)
                          )
                          (call $_abort)
                        )
                        (if
                          (i32.eq
                            (i32.load
                              (tee_local $10
                                (i32.add
                                  (get_local $0)
                                  (i32.const 8)
                                )
                              )
                            )
                            (get_local $3)
                          )
                          (block
                            (i32.store
                              (get_local $8)
                              (get_local $0)
                            )
                            (i32.store
                              (get_local $10)
                              (get_local $1)
                            )
                            (set_local $5
                              (get_local $0)
                            )
                          )
                          (call $_abort)
                        )
                      )
                    )
                  )
                  (block $do-once8
                    (if
                      (get_local $12)
                      (block
                        (if
                          (i32.eq
                            (get_local $3)
                            (i32.load
                              (tee_local $1
                                (i32.add
                                  (i32.shl
                                    (tee_local $0
                                      (i32.load offset=28
                                        (get_local $3)
                                      )
                                    )
                                    (i32.const 2)
                                  )
                                  (i32.const 5848)
                                )
                              )
                            )
                          )
                          (block
                            (i32.store
                              (get_local $1)
                              (get_local $5)
                            )
                            (if
                              (i32.eqz
                                (get_local $5)
                              )
                              (block
                                (i32.store
                                  (i32.const 5548)
                                  (i32.and
                                    (get_local $11)
                                    (i32.xor
                                      (i32.shl
                                        (i32.const 1)
                                        (get_local $0)
                                      )
                                      (i32.const -1)
                                    )
                                  )
                                )
                                (br $do-once8)
                              )
                            )
                          )
                          (block
                            (if
                              (i32.lt_u
                                (get_local $12)
                                (i32.load
                                  (i32.const 5560)
                                )
                              )
                              (call $_abort)
                            )
                            (if
                              (i32.eq
                                (i32.load
                                  (tee_local $0
                                    (i32.add
                                      (get_local $12)
                                      (i32.const 16)
                                    )
                                  )
                                )
                                (get_local $3)
                              )
                              (i32.store
                                (get_local $0)
                                (get_local $5)
                              )
                              (i32.store offset=20
                                (get_local $12)
                                (get_local $5)
                              )
                            )
                            (br_if $do-once8
                              (i32.eqz
                                (get_local $5)
                              )
                            )
                          )
                        )
                        (if
                          (i32.lt_u
                            (get_local $5)
                            (tee_local $1
                              (i32.load
                                (i32.const 5560)
                              )
                            )
                          )
                          (call $_abort)
                        )
                        (i32.store offset=24
                          (get_local $5)
                          (get_local $12)
                        )
                        (if
                          (tee_local $0
                            (i32.load offset=16
                              (get_local $3)
                            )
                          )
                          (if
                            (i32.lt_u
                              (get_local $0)
                              (get_local $1)
                            )
                            (call $_abort)
                            (block
                              (i32.store offset=16
                                (get_local $5)
                                (get_local $0)
                              )
                              (i32.store offset=24
                                (get_local $0)
                                (get_local $5)
                              )
                            )
                          )
                        )
                        (if
                          (tee_local $0
                            (i32.load offset=20
                              (get_local $3)
                            )
                          )
                          (if
                            (i32.lt_u
                              (get_local $0)
                              (i32.load
                                (i32.const 5560)
                              )
                            )
                            (call $_abort)
                            (block
                              (i32.store offset=20
                                (get_local $5)
                                (get_local $0)
                              )
                              (i32.store offset=24
                                (get_local $0)
                                (get_local $5)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                  (if
                    (i32.lt_u
                      (get_local $6)
                      (i32.const 16)
                    )
                    (block
                      (i32.store offset=4
                        (get_local $3)
                        (i32.or
                          (tee_local $0
                            (i32.add
                              (get_local $6)
                              (get_local $2)
                            )
                          )
                          (i32.const 3)
                        )
                      )
                      (i32.store
                        (tee_local $0
                          (i32.add
                            (i32.add
                              (get_local $3)
                              (get_local $0)
                            )
                            (i32.const 4)
                          )
                        )
                        (i32.or
                          (i32.load
                            (get_local $0)
                          )
                          (i32.const 1)
                        )
                      )
                    )
                    (block
                      (i32.store offset=4
                        (get_local $3)
                        (i32.or
                          (get_local $2)
                          (i32.const 3)
                        )
                      )
                      (i32.store offset=4
                        (get_local $9)
                        (i32.or
                          (get_local $6)
                          (i32.const 1)
                        )
                      )
                      (i32.store
                        (i32.add
                          (get_local $9)
                          (get_local $6)
                        )
                        (get_local $6)
                      )
                      (if
                        (get_local $16)
                        (block
                          (set_local $5
                            (i32.load
                              (i32.const 5564)
                            )
                          )
                          (set_local $0
                            (i32.add
                              (i32.shl
                                (i32.shl
                                  (tee_local $1
                                    (i32.shr_u
                                      (get_local $16)
                                      (i32.const 3)
                                    )
                                  )
                                  (i32.const 1)
                                )
                                (i32.const 2)
                              )
                              (i32.const 5584)
                            )
                          )
                          (if
                            (i32.and
                              (get_local $7)
                              (tee_local $1
                                (i32.shl
                                  (i32.const 1)
                                  (get_local $1)
                                )
                              )
                            )
                            (if
                              (i32.lt_u
                                (tee_local $2
                                  (i32.load
                                    (tee_local $1
                                      (i32.add
                                        (get_local $0)
                                        (i32.const 8)
                                      )
                                    )
                                  )
                                )
                                (i32.load
                                  (i32.const 5560)
                                )
                              )
                              (call $_abort)
                              (block
                                (set_local $4
                                  (get_local $2)
                                )
                                (set_local $14
                                  (get_local $1)
                                )
                              )
                            )
                            (block
                              (i32.store
                                (i32.const 5544)
                                (i32.or
                                  (get_local $7)
                                  (get_local $1)
                                )
                              )
                              (set_local $4
                                (get_local $0)
                              )
                              (set_local $14
                                (i32.add
                                  (get_local $0)
                                  (i32.const 8)
                                )
                              )
                            )
                          )
                          (i32.store
                            (get_local $14)
                            (get_local $5)
                          )
                          (i32.store offset=12
                            (get_local $4)
                            (get_local $5)
                          )
                          (i32.store offset=8
                            (get_local $5)
                            (get_local $4)
                          )
                          (i32.store offset=12
                            (get_local $5)
                            (get_local $0)
                          )
                        )
                      )
                      (i32.store
                        (i32.const 5552)
                        (get_local $6)
                      )
                      (i32.store
                        (i32.const 5564)
                        (get_local $9)
                      )
                    )
                  )
                  (set_global $STACKTOP
                    (get_local $13)
                  )
                  (return
                    (i32.add
                      (get_local $3)
                      (i32.const 8)
                    )
                  )
                )
                (set_local $0
                  (get_local $2)
                )
              )
            )
            (set_local $0
              (get_local $2)
            )
          )
        )
        (if
          (i32.gt_u
            (get_local $0)
            (i32.const -65)
          )
          (set_local $0
            (i32.const -1)
          )
          (block
            (set_local $5
              (i32.and
                (tee_local $0
                  (i32.add
                    (get_local $0)
                    (i32.const 11)
                  )
                )
                (i32.const -8)
              )
            )
            (if
              (tee_local $6
                (i32.load
                  (i32.const 5548)
                )
              )
              (block
                (set_local $17
                  (if i32
                    (tee_local $0
                      (i32.shr_u
                        (get_local $0)
                        (i32.const 8)
                      )
                    )
                    (if i32
                      (i32.gt_u
                        (get_local $5)
                        (i32.const 16777215)
                      )
                      (i32.const 31)
                      (i32.or
                        (i32.and
                          (i32.shr_u
                            (get_local $5)
                            (i32.add
                              (tee_local $0
                                (i32.add
                                  (i32.sub
                                    (i32.const 14)
                                    (i32.or
                                      (i32.or
                                        (tee_local $3
                                          (i32.and
                                            (i32.shr_u
                                              (i32.add
                                                (tee_local $2
                                                  (i32.shl
                                                    (get_local $0)
                                                    (tee_local $0
                                                      (i32.and
                                                        (i32.shr_u
                                                          (i32.add
                                                            (get_local $0)
                                                            (i32.const 1048320)
                                                          )
                                                          (i32.const 16)
                                                        )
                                                        (i32.const 8)
                                                      )
                                                    )
                                                  )
                                                )
                                                (i32.const 520192)
                                              )
                                              (i32.const 16)
                                            )
                                            (i32.const 4)
                                          )
                                        )
                                        (get_local $0)
                                      )
                                      (tee_local $2
                                        (i32.and
                                          (i32.shr_u
                                            (i32.add
                                              (tee_local $0
                                                (i32.shl
                                                  (get_local $2)
                                                  (get_local $3)
                                                )
                                              )
                                              (i32.const 245760)
                                            )
                                            (i32.const 16)
                                          )
                                          (i32.const 2)
                                        )
                                      )
                                    )
                                  )
                                  (i32.shr_u
                                    (i32.shl
                                      (get_local $0)
                                      (get_local $2)
                                    )
                                    (i32.const 15)
                                  )
                                )
                              )
                              (i32.const 7)
                            )
                          )
                          (i32.const 1)
                        )
                        (i32.shl
                          (get_local $0)
                          (i32.const 1)
                        )
                      )
                    )
                    (i32.const 0)
                  )
                )
                (set_local $3
                  (i32.sub
                    (i32.const 0)
                    (get_local $5)
                  )
                )
                (block $__rjto$3
                  (block $__rjti$3
                    (block $__rjti$2
                      (if
                        (tee_local $0
                          (i32.load
                            (i32.add
                              (i32.shl
                                (get_local $17)
                                (i32.const 2)
                              )
                              (i32.const 5848)
                            )
                          )
                        )
                        (block
                          (set_local $4
                            (i32.sub
                              (i32.const 25)
                              (i32.shr_u
                                (get_local $17)
                                (i32.const 1)
                              )
                            )
                          )
                          (set_local $2
                            (i32.const 0)
                          )
                          (set_local $11
                            (i32.shl
                              (get_local $5)
                              (if i32
                                (i32.eq
                                  (get_local $17)
                                  (i32.const 31)
                                )
                                (i32.const 0)
                                (get_local $4)
                              )
                            )
                          )
                          (set_local $4
                            (i32.const 0)
                          )
                          (loop $while-in14
                            (if
                              (i32.lt_u
                                (tee_local $14
                                  (i32.sub
                                    (i32.and
                                      (i32.load offset=4
                                        (get_local $0)
                                      )
                                      (i32.const -8)
                                    )
                                    (get_local $5)
                                  )
                                )
                                (get_local $3)
                              )
                              (if
                                (get_local $14)
                                (block
                                  (set_local $2
                                    (get_local $0)
                                  )
                                  (set_local $3
                                    (get_local $14)
                                  )
                                )
                                (block
                                  (set_local $2
                                    (get_local $0)
                                  )
                                  (set_local $3
                                    (i32.const 0)
                                  )
                                  (br $__rjti$3)
                                )
                              )
                            )
                            (if
                              (i32.eqz
                                (i32.or
                                  (i32.eqz
                                    (tee_local $14
                                      (i32.load offset=20
                                        (get_local $0)
                                      )
                                    )
                                  )
                                  (i32.eq
                                    (get_local $14)
                                    (tee_local $0
                                      (i32.load
                                        (i32.add
                                          (i32.add
                                            (get_local $0)
                                            (i32.const 16)
                                          )
                                          (i32.shl
                                            (i32.shr_u
                                              (get_local $11)
                                              (i32.const 31)
                                            )
                                            (i32.const 2)
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                              (set_local $4
                                (get_local $14)
                              )
                            )
                            (set_local $11
                              (i32.shl
                                (get_local $11)
                                (i32.xor
                                  (i32.and
                                    (tee_local $14
                                      (i32.eqz
                                        (get_local $0)
                                      )
                                    )
                                    (i32.const 1)
                                  )
                                  (i32.const 1)
                                )
                              )
                            )
                            (br_if $while-in14
                              (i32.eqz
                                (get_local $14)
                              )
                            )
                            (br $__rjti$2)
                          )
                        )
                        (block
                          (set_local $4
                            (i32.const 0)
                          )
                          (set_local $2
                            (i32.const 0)
                          )
                        )
                      )
                    )
                    (br_if $__rjti$3
                      (tee_local $0
                        (if i32
                          (i32.and
                            (i32.eqz
                              (get_local $4)
                            )
                            (i32.eqz
                              (get_local $2)
                            )
                          )
                          (block i32
                            (if
                              (i32.eqz
                                (tee_local $0
                                  (i32.and
                                    (get_local $6)
                                    (i32.or
                                      (tee_local $0
                                        (i32.shl
                                          (i32.const 2)
                                          (get_local $17)
                                        )
                                      )
                                      (i32.sub
                                        (i32.const 0)
                                        (get_local $0)
                                      )
                                    )
                                  )
                                )
                              )
                              (block
                                (set_local $0
                                  (get_local $5)
                                )
                                (br $do-once)
                              )
                            )
                            (set_local $0
                              (i32.and
                                (i32.shr_u
                                  (tee_local $4
                                    (i32.add
                                      (i32.and
                                        (get_local $0)
                                        (i32.sub
                                          (i32.const 0)
                                          (get_local $0)
                                        )
                                      )
                                      (i32.const -1)
                                    )
                                  )
                                  (i32.const 12)
                                )
                                (i32.const 16)
                              )
                            )
                            (i32.load
                              (i32.add
                                (i32.shl
                                  (i32.add
                                    (i32.or
                                      (i32.or
                                        (i32.or
                                          (i32.or
                                            (tee_local $11
                                              (i32.and
                                                (i32.shr_u
                                                  (tee_local $4
                                                    (i32.shr_u
                                                      (get_local $4)
                                                      (get_local $0)
                                                    )
                                                  )
                                                  (i32.const 5)
                                                )
                                                (i32.const 8)
                                              )
                                            )
                                            (get_local $0)
                                          )
                                          (tee_local $4
                                            (i32.and
                                              (i32.shr_u
                                                (tee_local $0
                                                  (i32.shr_u
                                                    (get_local $4)
                                                    (get_local $11)
                                                  )
                                                )
                                                (i32.const 2)
                                              )
                                              (i32.const 4)
                                            )
                                          )
                                        )
                                        (tee_local $4
                                          (i32.and
                                            (i32.shr_u
                                              (tee_local $0
                                                (i32.shr_u
                                                  (get_local $0)
                                                  (get_local $4)
                                                )
                                              )
                                              (i32.const 1)
                                            )
                                            (i32.const 2)
                                          )
                                        )
                                      )
                                      (tee_local $4
                                        (i32.and
                                          (i32.shr_u
                                            (tee_local $0
                                              (i32.shr_u
                                                (get_local $0)
                                                (get_local $4)
                                              )
                                            )
                                            (i32.const 1)
                                          )
                                          (i32.const 1)
                                        )
                                      )
                                    )
                                    (i32.shr_u
                                      (get_local $0)
                                      (get_local $4)
                                    )
                                  )
                                  (i32.const 2)
                                )
                                (i32.const 5848)
                              )
                            )
                          )
                          (get_local $4)
                        )
                      )
                    )
                    (set_local $4
                      (get_local $2)
                    )
                    (br $__rjto$3)
                  )
                  (loop $while-in16
                    (if
                      (tee_local $11
                        (i32.lt_u
                          (tee_local $4
                            (i32.sub
                              (i32.and
                                (i32.load offset=4
                                  (get_local $0)
                                )
                                (i32.const -8)
                              )
                              (get_local $5)
                            )
                          )
                          (get_local $3)
                        )
                      )
                      (set_local $3
                        (get_local $4)
                      )
                    )
                    (if
                      (get_local $11)
                      (set_local $2
                        (get_local $0)
                      )
                    )
                    (if
                      (tee_local $4
                        (i32.load offset=16
                          (get_local $0)
                        )
                      )
                      (block
                        (set_local $0
                          (get_local $4)
                        )
                        (br $while-in16)
                      )
                    )
                    (br_if $while-in16
                      (tee_local $0
                        (i32.load offset=20
                          (get_local $0)
                        )
                      )
                    )
                    (set_local $4
                      (get_local $2)
                    )
                  )
                )
                (if
                  (get_local $4)
                  (if
                    (i32.lt_u
                      (get_local $3)
                      (i32.sub
                        (i32.load
                          (i32.const 5552)
                        )
                        (get_local $5)
                      )
                    )
                    (block
                      (if
                        (i32.lt_u
                          (get_local $4)
                          (tee_local $15
                            (i32.load
                              (i32.const 5560)
                            )
                          )
                        )
                        (call $_abort)
                      )
                      (if
                        (i32.ge_u
                          (get_local $4)
                          (tee_local $9
                            (i32.add
                              (get_local $4)
                              (get_local $5)
                            )
                          )
                        )
                        (call $_abort)
                      )
                      (set_local $11
                        (i32.load offset=24
                          (get_local $4)
                        )
                      )
                      (block $do-once17
                        (if
                          (i32.eq
                            (tee_local $0
                              (i32.load offset=12
                                (get_local $4)
                              )
                            )
                            (get_local $4)
                          )
                          (block
                            (if
                              (i32.eqz
                                (tee_local $0
                                  (i32.load
                                    (tee_local $2
                                      (i32.add
                                        (get_local $4)
                                        (i32.const 20)
                                      )
                                    )
                                  )
                                )
                              )
                              (if
                                (i32.eqz
                                  (tee_local $0
                                    (i32.load
                                      (tee_local $2
                                        (i32.add
                                          (get_local $4)
                                          (i32.const 16)
                                        )
                                      )
                                    )
                                  )
                                )
                                (block
                                  (set_local $8
                                    (i32.const 0)
                                  )
                                  (br $do-once17)
                                )
                              )
                            )
                            (loop $while-in20
                              (if
                                (tee_local $12
                                  (i32.load
                                    (tee_local $10
                                      (i32.add
                                        (get_local $0)
                                        (i32.const 20)
                                      )
                                    )
                                  )
                                )
                                (block
                                  (set_local $0
                                    (get_local $12)
                                  )
                                  (set_local $2
                                    (get_local $10)
                                  )
                                  (br $while-in20)
                                )
                              )
                              (if
                                (tee_local $12
                                  (i32.load
                                    (tee_local $10
                                      (i32.add
                                        (get_local $0)
                                        (i32.const 16)
                                      )
                                    )
                                  )
                                )
                                (block
                                  (set_local $0
                                    (get_local $12)
                                  )
                                  (set_local $2
                                    (get_local $10)
                                  )
                                  (br $while-in20)
                                )
                              )
                            )
                            (if
                              (i32.lt_u
                                (get_local $2)
                                (get_local $15)
                              )
                              (call $_abort)
                              (block
                                (i32.store
                                  (get_local $2)
                                  (i32.const 0)
                                )
                                (set_local $8
                                  (get_local $0)
                                )
                              )
                            )
                          )
                          (block
                            (if
                              (i32.lt_u
                                (tee_local $2
                                  (i32.load offset=8
                                    (get_local $4)
                                  )
                                )
                                (get_local $15)
                              )
                              (call $_abort)
                            )
                            (if
                              (i32.ne
                                (i32.load
                                  (tee_local $10
                                    (i32.add
                                      (get_local $2)
                                      (i32.const 12)
                                    )
                                  )
                                )
                                (get_local $4)
                              )
                              (call $_abort)
                            )
                            (if
                              (i32.eq
                                (i32.load
                                  (tee_local $12
                                    (i32.add
                                      (get_local $0)
                                      (i32.const 8)
                                    )
                                  )
                                )
                                (get_local $4)
                              )
                              (block
                                (i32.store
                                  (get_local $10)
                                  (get_local $0)
                                )
                                (i32.store
                                  (get_local $12)
                                  (get_local $2)
                                )
                                (set_local $8
                                  (get_local $0)
                                )
                              )
                              (call $_abort)
                            )
                          )
                        )
                      )
                      (block $do-once21
                        (if
                          (get_local $11)
                          (block
                            (if
                              (i32.eq
                                (get_local $4)
                                (i32.load
                                  (tee_local $2
                                    (i32.add
                                      (i32.shl
                                        (tee_local $0
                                          (i32.load offset=28
                                            (get_local $4)
                                          )
                                        )
                                        (i32.const 2)
                                      )
                                      (i32.const 5848)
                                    )
                                  )
                                )
                              )
                              (block
                                (i32.store
                                  (get_local $2)
                                  (get_local $8)
                                )
                                (if
                                  (i32.eqz
                                    (get_local $8)
                                  )
                                  (block
                                    (i32.store
                                      (i32.const 5548)
                                      (tee_local $1
                                        (i32.and
                                          (get_local $6)
                                          (i32.xor
                                            (i32.shl
                                              (i32.const 1)
                                              (get_local $0)
                                            )
                                            (i32.const -1)
                                          )
                                        )
                                      )
                                    )
                                    (br $do-once21)
                                  )
                                )
                              )
                              (block
                                (if
                                  (i32.lt_u
                                    (get_local $11)
                                    (i32.load
                                      (i32.const 5560)
                                    )
                                  )
                                  (call $_abort)
                                )
                                (if
                                  (i32.eq
                                    (i32.load
                                      (tee_local $0
                                        (i32.add
                                          (get_local $11)
                                          (i32.const 16)
                                        )
                                      )
                                    )
                                    (get_local $4)
                                  )
                                  (i32.store
                                    (get_local $0)
                                    (get_local $8)
                                  )
                                  (i32.store offset=20
                                    (get_local $11)
                                    (get_local $8)
                                  )
                                )
                                (if
                                  (i32.eqz
                                    (get_local $8)
                                  )
                                  (block
                                    (set_local $1
                                      (get_local $6)
                                    )
                                    (br $do-once21)
                                  )
                                )
                              )
                            )
                            (if
                              (i32.lt_u
                                (get_local $8)
                                (tee_local $2
                                  (i32.load
                                    (i32.const 5560)
                                  )
                                )
                              )
                              (call $_abort)
                            )
                            (i32.store offset=24
                              (get_local $8)
                              (get_local $11)
                            )
                            (if
                              (tee_local $0
                                (i32.load offset=16
                                  (get_local $4)
                                )
                              )
                              (if
                                (i32.lt_u
                                  (get_local $0)
                                  (get_local $2)
                                )
                                (call $_abort)
                                (block
                                  (i32.store offset=16
                                    (get_local $8)
                                    (get_local $0)
                                  )
                                  (i32.store offset=24
                                    (get_local $0)
                                    (get_local $8)
                                  )
                                )
                              )
                            )
                            (if
                              (tee_local $0
                                (i32.load offset=20
                                  (get_local $4)
                                )
                              )
                              (if
                                (i32.lt_u
                                  (get_local $0)
                                  (i32.load
                                    (i32.const 5560)
                                  )
                                )
                                (call $_abort)
                                (block
                                  (i32.store offset=20
                                    (get_local $8)
                                    (get_local $0)
                                  )
                                  (i32.store offset=24
                                    (get_local $0)
                                    (get_local $8)
                                  )
                                  (set_local $1
                                    (get_local $6)
                                  )
                                )
                              )
                              (set_local $1
                                (get_local $6)
                              )
                            )
                          )
                          (set_local $1
                            (get_local $6)
                          )
                        )
                      )
                      (block $do-once25
                        (if
                          (i32.lt_u
                            (get_local $3)
                            (i32.const 16)
                          )
                          (block
                            (i32.store offset=4
                              (get_local $4)
                              (i32.or
                                (tee_local $0
                                  (i32.add
                                    (get_local $3)
                                    (get_local $5)
                                  )
                                )
                                (i32.const 3)
                              )
                            )
                            (i32.store
                              (tee_local $0
                                (i32.add
                                  (i32.add
                                    (get_local $4)
                                    (get_local $0)
                                  )
                                  (i32.const 4)
                                )
                              )
                              (i32.or
                                (i32.load
                                  (get_local $0)
                                )
                                (i32.const 1)
                              )
                            )
                          )
                          (block
                            (i32.store offset=4
                              (get_local $4)
                              (i32.or
                                (get_local $5)
                                (i32.const 3)
                              )
                            )
                            (i32.store offset=4
                              (get_local $9)
                              (i32.or
                                (get_local $3)
                                (i32.const 1)
                              )
                            )
                            (i32.store
                              (i32.add
                                (get_local $9)
                                (get_local $3)
                              )
                              (get_local $3)
                            )
                            (set_local $2
                              (i32.shr_u
                                (get_local $3)
                                (i32.const 3)
                              )
                            )
                            (if
                              (i32.lt_u
                                (get_local $3)
                                (i32.const 256)
                              )
                              (block
                                (set_local $0
                                  (i32.add
                                    (i32.shl
                                      (i32.shl
                                        (get_local $2)
                                        (i32.const 1)
                                      )
                                      (i32.const 2)
                                    )
                                    (i32.const 5584)
                                  )
                                )
                                (if
                                  (i32.and
                                    (tee_local $1
                                      (i32.load
                                        (i32.const 5544)
                                      )
                                    )
                                    (tee_local $2
                                      (i32.shl
                                        (i32.const 1)
                                        (get_local $2)
                                      )
                                    )
                                  )
                                  (if
                                    (i32.lt_u
                                      (tee_local $2
                                        (i32.load
                                          (tee_local $1
                                            (i32.add
                                              (get_local $0)
                                              (i32.const 8)
                                            )
                                          )
                                        )
                                      )
                                      (i32.load
                                        (i32.const 5560)
                                      )
                                    )
                                    (call $_abort)
                                    (block
                                      (set_local $7
                                        (get_local $2)
                                      )
                                      (set_local $16
                                        (get_local $1)
                                      )
                                    )
                                  )
                                  (block
                                    (i32.store
                                      (i32.const 5544)
                                      (i32.or
                                        (get_local $1)
                                        (get_local $2)
                                      )
                                    )
                                    (set_local $7
                                      (get_local $0)
                                    )
                                    (set_local $16
                                      (i32.add
                                        (get_local $0)
                                        (i32.const 8)
                                      )
                                    )
                                  )
                                )
                                (i32.store
                                  (get_local $16)
                                  (get_local $9)
                                )
                                (i32.store offset=12
                                  (get_local $7)
                                  (get_local $9)
                                )
                                (i32.store offset=8
                                  (get_local $9)
                                  (get_local $7)
                                )
                                (i32.store offset=12
                                  (get_local $9)
                                  (get_local $0)
                                )
                                (br $do-once25)
                              )
                            )
                            (set_local $0
                              (i32.add
                                (i32.shl
                                  (tee_local $2
                                    (if i32
                                      (tee_local $0
                                        (i32.shr_u
                                          (get_local $3)
                                          (i32.const 8)
                                        )
                                      )
                                      (if i32
                                        (i32.gt_u
                                          (get_local $3)
                                          (i32.const 16777215)
                                        )
                                        (i32.const 31)
                                        (i32.or
                                          (i32.and
                                            (i32.shr_u
                                              (get_local $3)
                                              (i32.add
                                                (tee_local $0
                                                  (i32.add
                                                    (i32.sub
                                                      (i32.const 14)
                                                      (i32.or
                                                        (i32.or
                                                          (tee_local $5
                                                            (i32.and
                                                              (i32.shr_u
                                                                (i32.add
                                                                  (tee_local $2
                                                                    (i32.shl
                                                                      (get_local $0)
                                                                      (tee_local $0
                                                                        (i32.and
                                                                          (i32.shr_u
                                                                            (i32.add
                                                                              (get_local $0)
                                                                              (i32.const 1048320)
                                                                            )
                                                                            (i32.const 16)
                                                                          )
                                                                          (i32.const 8)
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                  (i32.const 520192)
                                                                )
                                                                (i32.const 16)
                                                              )
                                                              (i32.const 4)
                                                            )
                                                          )
                                                          (get_local $0)
                                                        )
                                                        (tee_local $2
                                                          (i32.and
                                                            (i32.shr_u
                                                              (i32.add
                                                                (tee_local $0
                                                                  (i32.shl
                                                                    (get_local $2)
                                                                    (get_local $5)
                                                                  )
                                                                )
                                                                (i32.const 245760)
                                                              )
                                                              (i32.const 16)
                                                            )
                                                            (i32.const 2)
                                                          )
                                                        )
                                                      )
                                                    )
                                                    (i32.shr_u
                                                      (i32.shl
                                                        (get_local $0)
                                                        (get_local $2)
                                                      )
                                                      (i32.const 15)
                                                    )
                                                  )
                                                )
                                                (i32.const 7)
                                              )
                                            )
                                            (i32.const 1)
                                          )
                                          (i32.shl
                                            (get_local $0)
                                            (i32.const 1)
                                          )
                                        )
                                      )
                                      (i32.const 0)
                                    )
                                  )
                                  (i32.const 2)
                                )
                                (i32.const 5848)
                              )
                            )
                            (i32.store offset=28
                              (get_local $9)
                              (get_local $2)
                            )
                            (i32.store offset=4
                              (tee_local $5
                                (i32.add
                                  (get_local $9)
                                  (i32.const 16)
                                )
                              )
                              (i32.const 0)
                            )
                            (i32.store
                              (get_local $5)
                              (i32.const 0)
                            )
                            (if
                              (i32.eqz
                                (i32.and
                                  (get_local $1)
                                  (tee_local $5
                                    (i32.shl
                                      (i32.const 1)
                                      (get_local $2)
                                    )
                                  )
                                )
                              )
                              (block
                                (i32.store
                                  (i32.const 5548)
                                  (i32.or
                                    (get_local $1)
                                    (get_local $5)
                                  )
                                )
                                (i32.store
                                  (get_local $0)
                                  (get_local $9)
                                )
                                (i32.store offset=24
                                  (get_local $9)
                                  (get_local $0)
                                )
                                (i32.store offset=12
                                  (get_local $9)
                                  (get_local $9)
                                )
                                (i32.store offset=8
                                  (get_local $9)
                                  (get_local $9)
                                )
                                (br $do-once25)
                              )
                            )
                            (set_local $0
                              (i32.load
                                (get_local $0)
                              )
                            )
                            (set_local $1
                              (i32.sub
                                (i32.const 25)
                                (i32.shr_u
                                  (get_local $2)
                                  (i32.const 1)
                                )
                              )
                            )
                            (set_local $1
                              (i32.shl
                                (get_local $3)
                                (if i32
                                  (i32.eq
                                    (get_local $2)
                                    (i32.const 31)
                                  )
                                  (i32.const 0)
                                  (get_local $1)
                                )
                              )
                            )
                            (block $__rjto$1
                              (block $__rjti$1
                                (block $__rjti$0
                                  (loop $while-in28
                                    (br_if $__rjti$1
                                      (i32.eq
                                        (i32.and
                                          (i32.load offset=4
                                            (get_local $0)
                                          )
                                          (i32.const -8)
                                        )
                                        (get_local $3)
                                      )
                                    )
                                    (set_local $2
                                      (i32.shl
                                        (get_local $1)
                                        (i32.const 1)
                                      )
                                    )
                                    (br_if $__rjti$0
                                      (i32.eqz
                                        (tee_local $5
                                          (i32.load
                                            (tee_local $1
                                              (i32.add
                                                (i32.add
                                                  (get_local $0)
                                                  (i32.const 16)
                                                )
                                                (i32.shl
                                                  (i32.shr_u
                                                    (get_local $1)
                                                    (i32.const 31)
                                                  )
                                                  (i32.const 2)
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                    (set_local $1
                                      (get_local $2)
                                    )
                                    (set_local $0
                                      (get_local $5)
                                    )
                                    (br $while-in28)
                                  )
                                )
                                (if
                                  (i32.lt_u
                                    (get_local $1)
                                    (i32.load
                                      (i32.const 5560)
                                    )
                                  )
                                  (call $_abort)
                                  (block
                                    (i32.store
                                      (get_local $1)
                                      (get_local $9)
                                    )
                                    (i32.store offset=24
                                      (get_local $9)
                                      (get_local $0)
                                    )
                                    (i32.store offset=12
                                      (get_local $9)
                                      (get_local $9)
                                    )
                                    (i32.store offset=8
                                      (get_local $9)
                                      (get_local $9)
                                    )
                                    (br $do-once25)
                                  )
                                )
                                (br $__rjto$1)
                              )
                              (if
                                (i32.and
                                  (i32.ge_u
                                    (tee_local $1
                                      (i32.load
                                        (tee_local $2
                                          (i32.add
                                            (get_local $0)
                                            (i32.const 8)
                                          )
                                        )
                                      )
                                    )
                                    (tee_local $3
                                      (i32.load
                                        (i32.const 5560)
                                      )
                                    )
                                  )
                                  (i32.ge_u
                                    (get_local $0)
                                    (get_local $3)
                                  )
                                )
                                (block
                                  (i32.store offset=12
                                    (get_local $1)
                                    (get_local $9)
                                  )
                                  (i32.store
                                    (get_local $2)
                                    (get_local $9)
                                  )
                                  (i32.store offset=8
                                    (get_local $9)
                                    (get_local $1)
                                  )
                                  (i32.store offset=12
                                    (get_local $9)
                                    (get_local $0)
                                  )
                                  (i32.store offset=24
                                    (get_local $9)
                                    (i32.const 0)
                                  )
                                )
                                (call $_abort)
                              )
                            )
                          )
                        )
                      )
                      (set_global $STACKTOP
                        (get_local $13)
                      )
                      (return
                        (i32.add
                          (get_local $4)
                          (i32.const 8)
                        )
                      )
                    )
                    (set_local $0
                      (get_local $5)
                    )
                  )
                  (set_local $0
                    (get_local $5)
                  )
                )
              )
              (set_local $0
                (get_local $5)
              )
            )
          )
        )
      )
    )
    (if
      (i32.ge_u
        (tee_local $3
          (i32.load
            (i32.const 5552)
          )
        )
        (get_local $0)
      )
      (block
        (set_local $1
          (i32.load
            (i32.const 5564)
          )
        )
        (if
          (i32.gt_u
            (tee_local $2
              (i32.sub
                (get_local $3)
                (get_local $0)
              )
            )
            (i32.const 15)
          )
          (block
            (i32.store
              (i32.const 5564)
              (tee_local $3
                (i32.add
                  (get_local $1)
                  (get_local $0)
                )
              )
            )
            (i32.store
              (i32.const 5552)
              (get_local $2)
            )
            (i32.store offset=4
              (get_local $3)
              (i32.or
                (get_local $2)
                (i32.const 1)
              )
            )
            (i32.store
              (i32.add
                (get_local $3)
                (get_local $2)
              )
              (get_local $2)
            )
            (i32.store offset=4
              (get_local $1)
              (i32.or
                (get_local $0)
                (i32.const 3)
              )
            )
          )
          (block
            (i32.store
              (i32.const 5552)
              (i32.const 0)
            )
            (i32.store
              (i32.const 5564)
              (i32.const 0)
            )
            (i32.store offset=4
              (get_local $1)
              (i32.or
                (get_local $3)
                (i32.const 3)
              )
            )
            (i32.store
              (tee_local $0
                (i32.add
                  (i32.add
                    (get_local $1)
                    (get_local $3)
                  )
                  (i32.const 4)
                )
              )
              (i32.or
                (i32.load
                  (get_local $0)
                )
                (i32.const 1)
              )
            )
          )
        )
        (set_global $STACKTOP
          (get_local $13)
        )
        (return
          (i32.add
            (get_local $1)
            (i32.const 8)
          )
        )
      )
    )
    (if
      (i32.gt_u
        (tee_local $3
          (i32.load
            (i32.const 5556)
          )
        )
        (get_local $0)
      )
      (block
        (i32.store
          (i32.const 5556)
          (tee_local $2
            (i32.sub
              (get_local $3)
              (get_local $0)
            )
          )
        )
        (i32.store
          (i32.const 5568)
          (tee_local $3
            (i32.add
              (tee_local $1
                (i32.load
                  (i32.const 5568)
                )
              )
              (get_local $0)
            )
          )
        )
        (i32.store offset=4
          (get_local $3)
          (i32.or
            (get_local $2)
            (i32.const 1)
          )
        )
        (i32.store offset=4
          (get_local $1)
          (i32.or
            (get_local $0)
            (i32.const 3)
          )
        )
        (set_global $STACKTOP
          (get_local $13)
        )
        (return
          (i32.add
            (get_local $1)
            (i32.const 8)
          )
        )
      )
    )
    (if
      (i32.le_u
        (tee_local $5
          (i32.and
            (tee_local $4
              (i32.add
                (tee_local $1
                  (if i32
                    (i32.load
                      (i32.const 6016)
                    )
                    (i32.load
                      (i32.const 6024)
                    )
                    (block i32
                      (i32.store
                        (i32.const 6024)
                        (i32.const 4096)
                      )
                      (i32.store
                        (i32.const 6020)
                        (i32.const 4096)
                      )
                      (i32.store
                        (i32.const 6028)
                        (i32.const -1)
                      )
                      (i32.store
                        (i32.const 6032)
                        (i32.const -1)
                      )
                      (i32.store
                        (i32.const 6036)
                        (i32.const 0)
                      )
                      (i32.store
                        (i32.const 5988)
                        (i32.const 0)
                      )
                      (i32.store
                        (get_local $15)
                        (tee_local $1
                          (i32.xor
                            (i32.and
                              (get_local $15)
                              (i32.const -16)
                            )
                            (i32.const 1431655768)
                          )
                        )
                      )
                      (i32.store
                        (i32.const 6016)
                        (get_local $1)
                      )
                      (i32.const 4096)
                    )
                  )
                )
                (tee_local $6
                  (i32.add
                    (get_local $0)
                    (i32.const 47)
                  )
                )
              )
            )
            (tee_local $8
              (i32.sub
                (i32.const 0)
                (get_local $1)
              )
            )
          )
        )
        (get_local $0)
      )
      (block
        (set_global $STACKTOP
          (get_local $13)
        )
        (return
          (i32.const 0)
        )
      )
    )
    (if
      (tee_local $1
        (i32.load
          (i32.const 5984)
        )
      )
      (if
        (i32.or
          (i32.le_u
            (tee_local $7
              (i32.add
                (tee_local $2
                  (i32.load
                    (i32.const 5976)
                  )
                )
                (get_local $5)
              )
            )
            (get_local $2)
          )
          (i32.gt_u
            (get_local $7)
            (get_local $1)
          )
        )
        (block
          (set_global $STACKTOP
            (get_local $13)
          )
          (return
            (i32.const 0)
          )
        )
      )
    )
    (set_local $7
      (i32.add
        (get_local $0)
        (i32.const 48)
      )
    )
    (block $__rjto$13
      (block $__rjti$13
        (if
          (i32.eqz
            (i32.and
              (i32.load
                (i32.const 5988)
              )
              (i32.const 4)
            )
          )
          (block
            (block $label$break$L274
              (block $__rjti$5
                (block $__rjti$4
                  (br_if $__rjti$4
                    (i32.eqz
                      (tee_local $1
                        (i32.load
                          (i32.const 5568)
                        )
                      )
                    )
                  )
                  (set_local $2
                    (i32.const 5992)
                  )
                  (loop $while-in32
                    (block $while-out31
                      (if
                        (i32.le_u
                          (tee_local $11
                            (i32.load
                              (get_local $2)
                            )
                          )
                          (get_local $1)
                        )
                        (br_if $while-out31
                          (i32.gt_u
                            (i32.add
                              (get_local $11)
                              (i32.load
                                (tee_local $11
                                  (i32.add
                                    (get_local $2)
                                    (i32.const 4)
                                  )
                                )
                              )
                            )
                            (get_local $1)
                          )
                        )
                      )
                      (br_if $while-in32
                        (tee_local $2
                          (i32.load offset=8
                            (get_local $2)
                          )
                        )
                      )
                      (br $__rjti$4)
                    )
                  )
                  (if
                    (i32.lt_u
                      (tee_local $1
                        (i32.and
                          (i32.sub
                            (get_local $4)
                            (get_local $3)
                          )
                          (get_local $8)
                        )
                      )
                      (i32.const 2147483647)
                    )
                    (if
                      (i32.eq
                        (tee_local $3
                          (call $_sbrk
                            (get_local $1)
                          )
                        )
                        (i32.add
                          (i32.load
                            (get_local $2)
                          )
                          (i32.load
                            (get_local $11)
                          )
                        )
                      )
                      (if
                        (i32.ne
                          (get_local $3)
                          (i32.const -1)
                        )
                        (block
                          (set_local $2
                            (get_local $1)
                          )
                          (set_local $1
                            (get_local $3)
                          )
                          (br $__rjti$13)
                        )
                      )
                      (br $__rjti$5)
                    )
                  )
                  (br $label$break$L274)
                )
                (if
                  (i32.ne
                    (tee_local $3
                      (call $_sbrk
                        (i32.const 0)
                      )
                    )
                    (i32.const -1)
                  )
                  (block
                    (set_local $2
                      (i32.sub
                        (i32.and
                          (i32.add
                            (tee_local $4
                              (i32.add
                                (tee_local $2
                                  (i32.load
                                    (i32.const 6020)
                                  )
                                )
                                (i32.const -1)
                              )
                            )
                            (tee_local $1
                              (get_local $3)
                            )
                          )
                          (i32.sub
                            (i32.const 0)
                            (get_local $2)
                          )
                        )
                        (get_local $1)
                      )
                    )
                    (set_local $2
                      (i32.add
                        (tee_local $1
                          (i32.add
                            (if i32
                              (i32.and
                                (get_local $4)
                                (get_local $1)
                              )
                              (get_local $2)
                              (i32.const 0)
                            )
                            (get_local $5)
                          )
                        )
                        (tee_local $4
                          (i32.load
                            (i32.const 5976)
                          )
                        )
                      )
                    )
                    (if
                      (i32.and
                        (i32.gt_u
                          (get_local $1)
                          (get_local $0)
                        )
                        (i32.lt_u
                          (get_local $1)
                          (i32.const 2147483647)
                        )
                      )
                      (block
                        (if
                          (tee_local $8
                            (i32.load
                              (i32.const 5984)
                            )
                          )
                          (br_if $label$break$L274
                            (i32.or
                              (i32.le_u
                                (get_local $2)
                                (get_local $4)
                              )
                              (i32.gt_u
                                (get_local $2)
                                (get_local $8)
                              )
                            )
                          )
                        )
                        (if
                          (i32.eq
                            (tee_local $2
                              (call $_sbrk
                                (get_local $1)
                              )
                            )
                            (get_local $3)
                          )
                          (block
                            (set_local $2
                              (get_local $1)
                            )
                            (set_local $1
                              (get_local $3)
                            )
                            (br $__rjti$13)
                          )
                          (block
                            (set_local $3
                              (get_local $2)
                            )
                            (br $__rjti$5)
                          )
                        )
                      )
                    )
                  )
                )
                (br $label$break$L274)
              )
              (set_local $4
                (i32.sub
                  (i32.const 0)
                  (get_local $1)
                )
              )
              (if
                (i32.and
                  (i32.gt_u
                    (get_local $7)
                    (get_local $1)
                  )
                  (i32.and
                    (i32.lt_u
                      (get_local $1)
                      (i32.const 2147483647)
                    )
                    (i32.ne
                      (get_local $3)
                      (i32.const -1)
                    )
                  )
                )
                (if
                  (i32.lt_u
                    (tee_local $2
                      (i32.and
                        (i32.add
                          (i32.sub
                            (get_local $6)
                            (get_local $1)
                          )
                          (tee_local $2
                            (i32.load
                              (i32.const 6024)
                            )
                          )
                        )
                        (i32.sub
                          (i32.const 0)
                          (get_local $2)
                        )
                      )
                    )
                    (i32.const 2147483647)
                  )
                  (if
                    (i32.eq
                      (call $_sbrk
                        (get_local $2)
                      )
                      (i32.const -1)
                    )
                    (block
                      (drop
                        (call $_sbrk
                          (get_local $4)
                        )
                      )
                      (br $label$break$L274)
                    )
                    (set_local $1
                      (i32.add
                        (get_local $2)
                        (get_local $1)
                      )
                    )
                  )
                )
              )
              (if
                (i32.ne
                  (get_local $3)
                  (i32.const -1)
                )
                (block
                  (set_local $2
                    (get_local $1)
                  )
                  (set_local $1
                    (get_local $3)
                  )
                  (br $__rjti$13)
                )
              )
            )
            (i32.store
              (i32.const 5988)
              (i32.or
                (i32.load
                  (i32.const 5988)
                )
                (i32.const 4)
              )
            )
          )
        )
        (if
          (i32.lt_u
            (get_local $5)
            (i32.const 2147483647)
          )
          (if
            (i32.and
              (i32.lt_u
                (tee_local $1
                  (call $_sbrk
                    (get_local $5)
                  )
                )
                (tee_local $2
                  (call $_sbrk
                    (i32.const 0)
                  )
                )
              )
              (i32.and
                (i32.ne
                  (get_local $1)
                  (i32.const -1)
                )
                (i32.ne
                  (get_local $2)
                  (i32.const -1)
                )
              )
            )
            (br_if $__rjti$13
              (i32.gt_u
                (tee_local $2
                  (i32.sub
                    (get_local $2)
                    (get_local $1)
                  )
                )
                (i32.add
                  (get_local $0)
                  (i32.const 40)
                )
              )
            )
          )
        )
        (br $__rjto$13)
      )
      (i32.store
        (i32.const 5976)
        (tee_local $3
          (i32.add
            (i32.load
              (i32.const 5976)
            )
            (get_local $2)
          )
        )
      )
      (if
        (i32.gt_u
          (get_local $3)
          (i32.load
            (i32.const 5980)
          )
        )
        (i32.store
          (i32.const 5980)
          (get_local $3)
        )
      )
      (block $do-once38
        (if
          (tee_local $6
            (i32.load
              (i32.const 5568)
            )
          )
          (block
            (set_local $3
              (i32.const 5992)
            )
            (block $__rjto$10
              (block $__rjti$10
                (loop $while-in43
                  (br_if $__rjti$10
                    (i32.eq
                      (get_local $1)
                      (i32.add
                        (tee_local $5
                          (i32.load
                            (get_local $3)
                          )
                        )
                        (tee_local $8
                          (i32.load
                            (tee_local $4
                              (i32.add
                                (get_local $3)
                                (i32.const 4)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                  (br_if $while-in43
                    (tee_local $3
                      (i32.load offset=8
                        (get_local $3)
                      )
                    )
                  )
                )
                (br $__rjto$10)
              )
              (if
                (i32.eqz
                  (i32.and
                    (i32.load offset=12
                      (get_local $3)
                    )
                    (i32.const 8)
                  )
                )
                (if
                  (i32.and
                    (i32.lt_u
                      (get_local $6)
                      (get_local $1)
                    )
                    (i32.ge_u
                      (get_local $6)
                      (get_local $5)
                    )
                  )
                  (block
                    (i32.store
                      (get_local $4)
                      (i32.add
                        (get_local $8)
                        (get_local $2)
                      )
                    )
                    (set_local $5
                      (i32.load
                        (i32.const 5556)
                      )
                    )
                    (set_local $1
                      (i32.and
                        (i32.sub
                          (i32.const 0)
                          (tee_local $3
                            (i32.add
                              (get_local $6)
                              (i32.const 8)
                            )
                          )
                        )
                        (i32.const 7)
                      )
                    )
                    (i32.store
                      (i32.const 5568)
                      (tee_local $3
                        (i32.add
                          (get_local $6)
                          (if i32
                            (i32.and
                              (get_local $3)
                              (i32.const 7)
                            )
                            (get_local $1)
                            (tee_local $1
                              (i32.const 0)
                            )
                          )
                        )
                      )
                    )
                    (i32.store
                      (i32.const 5556)
                      (tee_local $1
                        (i32.add
                          (i32.sub
                            (get_local $2)
                            (get_local $1)
                          )
                          (get_local $5)
                        )
                      )
                    )
                    (i32.store offset=4
                      (get_local $3)
                      (i32.or
                        (get_local $1)
                        (i32.const 1)
                      )
                    )
                    (i32.store offset=4
                      (i32.add
                        (get_local $3)
                        (get_local $1)
                      )
                      (i32.const 40)
                    )
                    (i32.store
                      (i32.const 5572)
                      (i32.load
                        (i32.const 6032)
                      )
                    )
                    (br $do-once38)
                  )
                )
              )
            )
            (if
              (i32.lt_u
                (get_local $1)
                (tee_local $3
                  (i32.load
                    (i32.const 5560)
                  )
                )
              )
              (block
                (i32.store
                  (i32.const 5560)
                  (get_local $1)
                )
                (set_local $3
                  (get_local $1)
                )
              )
            )
            (set_local $4
              (i32.add
                (get_local $1)
                (get_local $2)
              )
            )
            (set_local $5
              (i32.const 5992)
            )
            (block $__rjto$11
              (block $__rjti$11
                (loop $while-in45
                  (br_if $__rjti$11
                    (i32.eq
                      (i32.load
                        (get_local $5)
                      )
                      (get_local $4)
                    )
                  )
                  (br_if $while-in45
                    (tee_local $5
                      (i32.load offset=8
                        (get_local $5)
                      )
                    )
                  )
                  (set_local $3
                    (i32.const 5992)
                  )
                )
                (br $__rjto$11)
              )
              (if
                (i32.and
                  (i32.load offset=12
                    (get_local $5)
                  )
                  (i32.const 8)
                )
                (set_local $3
                  (i32.const 5992)
                )
                (block
                  (i32.store
                    (get_local $5)
                    (get_local $1)
                  )
                  (i32.store
                    (tee_local $5
                      (i32.add
                        (get_local $5)
                        (i32.const 4)
                      )
                    )
                    (i32.add
                      (i32.load
                        (get_local $5)
                      )
                      (get_local $2)
                    )
                  )
                  (set_local $5
                    (i32.and
                      (i32.sub
                        (i32.const 0)
                        (tee_local $2
                          (i32.add
                            (get_local $1)
                            (i32.const 8)
                          )
                        )
                      )
                      (i32.const 7)
                    )
                  )
                  (set_local $11
                    (i32.and
                      (i32.sub
                        (i32.const 0)
                        (tee_local $8
                          (i32.add
                            (get_local $4)
                            (i32.const 8)
                          )
                        )
                      )
                      (i32.const 7)
                    )
                  )
                  (set_local $7
                    (i32.add
                      (tee_local $9
                        (i32.add
                          (get_local $1)
                          (if i32
                            (i32.and
                              (get_local $2)
                              (i32.const 7)
                            )
                            (get_local $5)
                            (i32.const 0)
                          )
                        )
                      )
                      (get_local $0)
                    )
                  )
                  (set_local $8
                    (i32.sub
                      (i32.sub
                        (tee_local $4
                          (i32.add
                            (get_local $4)
                            (if i32
                              (i32.and
                                (get_local $8)
                                (i32.const 7)
                              )
                              (get_local $11)
                              (i32.const 0)
                            )
                          )
                        )
                        (get_local $9)
                      )
                      (get_local $0)
                    )
                  )
                  (i32.store offset=4
                    (get_local $9)
                    (i32.or
                      (get_local $0)
                      (i32.const 3)
                    )
                  )
                  (block $do-once46
                    (if
                      (i32.eq
                        (get_local $4)
                        (get_local $6)
                      )
                      (block
                        (i32.store
                          (i32.const 5556)
                          (tee_local $0
                            (i32.add
                              (i32.load
                                (i32.const 5556)
                              )
                              (get_local $8)
                            )
                          )
                        )
                        (i32.store
                          (i32.const 5568)
                          (get_local $7)
                        )
                        (i32.store offset=4
                          (get_local $7)
                          (i32.or
                            (get_local $0)
                            (i32.const 1)
                          )
                        )
                      )
                      (block
                        (if
                          (i32.eq
                            (get_local $4)
                            (i32.load
                              (i32.const 5564)
                            )
                          )
                          (block
                            (i32.store
                              (i32.const 5552)
                              (tee_local $0
                                (i32.add
                                  (i32.load
                                    (i32.const 5552)
                                  )
                                  (get_local $8)
                                )
                              )
                            )
                            (i32.store
                              (i32.const 5564)
                              (get_local $7)
                            )
                            (i32.store offset=4
                              (get_local $7)
                              (i32.or
                                (get_local $0)
                                (i32.const 1)
                              )
                            )
                            (i32.store
                              (i32.add
                                (get_local $7)
                                (get_local $0)
                              )
                              (get_local $0)
                            )
                            (br $do-once46)
                          )
                        )
                        (set_local $5
                          (if i32
                            (i32.eq
                              (i32.and
                                (tee_local $0
                                  (i32.load offset=4
                                    (get_local $4)
                                  )
                                )
                                (i32.const 3)
                              )
                              (i32.const 1)
                            )
                            (block i32
                              (set_local $11
                                (i32.and
                                  (get_local $0)
                                  (i32.const -8)
                                )
                              )
                              (set_local $5
                                (i32.shr_u
                                  (get_local $0)
                                  (i32.const 3)
                                )
                              )
                              (block $label$break$L326
                                (if
                                  (i32.lt_u
                                    (get_local $0)
                                    (i32.const 256)
                                  )
                                  (block
                                    (set_local $1
                                      (i32.load offset=12
                                        (get_local $4)
                                      )
                                    )
                                    (block $do-once49
                                      (if
                                        (i32.ne
                                          (tee_local $2
                                            (i32.load offset=8
                                              (get_local $4)
                                            )
                                          )
                                          (tee_local $0
                                            (i32.add
                                              (i32.shl
                                                (i32.shl
                                                  (get_local $5)
                                                  (i32.const 1)
                                                )
                                                (i32.const 2)
                                              )
                                              (i32.const 5584)
                                            )
                                          )
                                        )
                                        (block
                                          (if
                                            (i32.lt_u
                                              (get_local $2)
                                              (get_local $3)
                                            )
                                            (call $_abort)
                                          )
                                          (br_if $do-once49
                                            (i32.eq
                                              (i32.load offset=12
                                                (get_local $2)
                                              )
                                              (get_local $4)
                                            )
                                          )
                                          (call $_abort)
                                        )
                                      )
                                    )
                                    (if
                                      (i32.eq
                                        (get_local $1)
                                        (get_local $2)
                                      )
                                      (block
                                        (i32.store
                                          (i32.const 5544)
                                          (i32.and
                                            (i32.load
                                              (i32.const 5544)
                                            )
                                            (i32.xor
                                              (i32.shl
                                                (i32.const 1)
                                                (get_local $5)
                                              )
                                              (i32.const -1)
                                            )
                                          )
                                        )
                                        (br $label$break$L326)
                                      )
                                    )
                                    (block $do-once51
                                      (if
                                        (i32.eq
                                          (get_local $1)
                                          (get_local $0)
                                        )
                                        (set_local $18
                                          (i32.add
                                            (get_local $1)
                                            (i32.const 8)
                                          )
                                        )
                                        (block
                                          (if
                                            (i32.lt_u
                                              (get_local $1)
                                              (get_local $3)
                                            )
                                            (call $_abort)
                                          )
                                          (if
                                            (i32.eq
                                              (i32.load
                                                (tee_local $0
                                                  (i32.add
                                                    (get_local $1)
                                                    (i32.const 8)
                                                  )
                                                )
                                              )
                                              (get_local $4)
                                            )
                                            (block
                                              (set_local $18
                                                (get_local $0)
                                              )
                                              (br $do-once51)
                                            )
                                          )
                                          (call $_abort)
                                        )
                                      )
                                    )
                                    (i32.store offset=12
                                      (get_local $2)
                                      (get_local $1)
                                    )
                                    (i32.store
                                      (get_local $18)
                                      (get_local $2)
                                    )
                                  )
                                  (block
                                    (set_local $6
                                      (i32.load offset=24
                                        (get_local $4)
                                      )
                                    )
                                    (block $do-once53
                                      (if
                                        (i32.eq
                                          (tee_local $0
                                            (i32.load offset=12
                                              (get_local $4)
                                            )
                                          )
                                          (get_local $4)
                                        )
                                        (block
                                          (if
                                            (tee_local $0
                                              (i32.load
                                                (tee_local $2
                                                  (i32.add
                                                    (tee_local $1
                                                      (i32.add
                                                        (get_local $4)
                                                        (i32.const 16)
                                                      )
                                                    )
                                                    (i32.const 4)
                                                  )
                                                )
                                              )
                                            )
                                            (set_local $1
                                              (get_local $2)
                                            )
                                            (if
                                              (i32.eqz
                                                (tee_local $0
                                                  (i32.load
                                                    (get_local $1)
                                                  )
                                                )
                                              )
                                              (block
                                                (set_local $10
                                                  (i32.const 0)
                                                )
                                                (br $do-once53)
                                              )
                                            )
                                          )
                                          (loop $while-in56
                                            (if
                                              (tee_local $5
                                                (i32.load
                                                  (tee_local $2
                                                    (i32.add
                                                      (get_local $0)
                                                      (i32.const 20)
                                                    )
                                                  )
                                                )
                                              )
                                              (block
                                                (set_local $0
                                                  (get_local $5)
                                                )
                                                (set_local $1
                                                  (get_local $2)
                                                )
                                                (br $while-in56)
                                              )
                                            )
                                            (if
                                              (tee_local $5
                                                (i32.load
                                                  (tee_local $2
                                                    (i32.add
                                                      (get_local $0)
                                                      (i32.const 16)
                                                    )
                                                  )
                                                )
                                              )
                                              (block
                                                (set_local $0
                                                  (get_local $5)
                                                )
                                                (set_local $1
                                                  (get_local $2)
                                                )
                                                (br $while-in56)
                                              )
                                            )
                                          )
                                          (if
                                            (i32.lt_u
                                              (get_local $1)
                                              (get_local $3)
                                            )
                                            (call $_abort)
                                            (block
                                              (i32.store
                                                (get_local $1)
                                                (i32.const 0)
                                              )
                                              (set_local $10
                                                (get_local $0)
                                              )
                                            )
                                          )
                                        )
                                        (block
                                          (if
                                            (i32.lt_u
                                              (tee_local $1
                                                (i32.load offset=8
                                                  (get_local $4)
                                                )
                                              )
                                              (get_local $3)
                                            )
                                            (call $_abort)
                                          )
                                          (if
                                            (i32.ne
                                              (i32.load
                                                (tee_local $2
                                                  (i32.add
                                                    (get_local $1)
                                                    (i32.const 12)
                                                  )
                                                )
                                              )
                                              (get_local $4)
                                            )
                                            (call $_abort)
                                          )
                                          (if
                                            (i32.eq
                                              (i32.load
                                                (tee_local $3
                                                  (i32.add
                                                    (get_local $0)
                                                    (i32.const 8)
                                                  )
                                                )
                                              )
                                              (get_local $4)
                                            )
                                            (block
                                              (i32.store
                                                (get_local $2)
                                                (get_local $0)
                                              )
                                              (i32.store
                                                (get_local $3)
                                                (get_local $1)
                                              )
                                              (set_local $10
                                                (get_local $0)
                                              )
                                            )
                                            (call $_abort)
                                          )
                                        )
                                      )
                                    )
                                    (br_if $label$break$L326
                                      (i32.eqz
                                        (get_local $6)
                                      )
                                    )
                                    (block $do-once57
                                      (if
                                        (i32.eq
                                          (get_local $4)
                                          (i32.load
                                            (tee_local $1
                                              (i32.add
                                                (i32.shl
                                                  (tee_local $0
                                                    (i32.load offset=28
                                                      (get_local $4)
                                                    )
                                                  )
                                                  (i32.const 2)
                                                )
                                                (i32.const 5848)
                                              )
                                            )
                                          )
                                        )
                                        (block
                                          (i32.store
                                            (get_local $1)
                                            (get_local $10)
                                          )
                                          (br_if $do-once57
                                            (get_local $10)
                                          )
                                          (i32.store
                                            (i32.const 5548)
                                            (i32.and
                                              (i32.load
                                                (i32.const 5548)
                                              )
                                              (i32.xor
                                                (i32.shl
                                                  (i32.const 1)
                                                  (get_local $0)
                                                )
                                                (i32.const -1)
                                              )
                                            )
                                          )
                                          (br $label$break$L326)
                                        )
                                        (block
                                          (if
                                            (i32.lt_u
                                              (get_local $6)
                                              (i32.load
                                                (i32.const 5560)
                                              )
                                            )
                                            (call $_abort)
                                          )
                                          (if
                                            (i32.eq
                                              (i32.load
                                                (tee_local $0
                                                  (i32.add
                                                    (get_local $6)
                                                    (i32.const 16)
                                                  )
                                                )
                                              )
                                              (get_local $4)
                                            )
                                            (i32.store
                                              (get_local $0)
                                              (get_local $10)
                                            )
                                            (i32.store offset=20
                                              (get_local $6)
                                              (get_local $10)
                                            )
                                          )
                                          (br_if $label$break$L326
                                            (i32.eqz
                                              (get_local $10)
                                            )
                                          )
                                        )
                                      )
                                    )
                                    (if
                                      (i32.lt_u
                                        (get_local $10)
                                        (tee_local $1
                                          (i32.load
                                            (i32.const 5560)
                                          )
                                        )
                                      )
                                      (call $_abort)
                                    )
                                    (i32.store offset=24
                                      (get_local $10)
                                      (get_local $6)
                                    )
                                    (if
                                      (tee_local $0
                                        (i32.load
                                          (tee_local $2
                                            (i32.add
                                              (get_local $4)
                                              (i32.const 16)
                                            )
                                          )
                                        )
                                      )
                                      (if
                                        (i32.lt_u
                                          (get_local $0)
                                          (get_local $1)
                                        )
                                        (call $_abort)
                                        (block
                                          (i32.store offset=16
                                            (get_local $10)
                                            (get_local $0)
                                          )
                                          (i32.store offset=24
                                            (get_local $0)
                                            (get_local $10)
                                          )
                                        )
                                      )
                                    )
                                    (br_if $label$break$L326
                                      (i32.eqz
                                        (tee_local $0
                                          (i32.load offset=4
                                            (get_local $2)
                                          )
                                        )
                                      )
                                    )
                                    (if
                                      (i32.lt_u
                                        (get_local $0)
                                        (i32.load
                                          (i32.const 5560)
                                        )
                                      )
                                      (call $_abort)
                                      (block
                                        (i32.store offset=20
                                          (get_local $10)
                                          (get_local $0)
                                        )
                                        (i32.store offset=24
                                          (get_local $0)
                                          (get_local $10)
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                              (set_local $4
                                (i32.add
                                  (get_local $4)
                                  (get_local $11)
                                )
                              )
                              (i32.add
                                (get_local $11)
                                (get_local $8)
                              )
                            )
                            (get_local $8)
                          )
                        )
                        (i32.store
                          (tee_local $0
                            (i32.add
                              (get_local $4)
                              (i32.const 4)
                            )
                          )
                          (i32.and
                            (i32.load
                              (get_local $0)
                            )
                            (i32.const -2)
                          )
                        )
                        (i32.store offset=4
                          (get_local $7)
                          (i32.or
                            (get_local $5)
                            (i32.const 1)
                          )
                        )
                        (i32.store
                          (i32.add
                            (get_local $7)
                            (get_local $5)
                          )
                          (get_local $5)
                        )
                        (set_local $1
                          (i32.shr_u
                            (get_local $5)
                            (i32.const 3)
                          )
                        )
                        (if
                          (i32.lt_u
                            (get_local $5)
                            (i32.const 256)
                          )
                          (block
                            (set_local $0
                              (i32.add
                                (i32.shl
                                  (i32.shl
                                    (get_local $1)
                                    (i32.const 1)
                                  )
                                  (i32.const 2)
                                )
                                (i32.const 5584)
                              )
                            )
                            (block $do-once61
                              (if
                                (i32.and
                                  (tee_local $2
                                    (i32.load
                                      (i32.const 5544)
                                    )
                                  )
                                  (tee_local $1
                                    (i32.shl
                                      (i32.const 1)
                                      (get_local $1)
                                    )
                                  )
                                )
                                (block
                                  (if
                                    (i32.ge_u
                                      (tee_local $2
                                        (i32.load
                                          (tee_local $1
                                            (i32.add
                                              (get_local $0)
                                              (i32.const 8)
                                            )
                                          )
                                        )
                                      )
                                      (i32.load
                                        (i32.const 5560)
                                      )
                                    )
                                    (block
                                      (set_local $12
                                        (get_local $2)
                                      )
                                      (set_local $19
                                        (get_local $1)
                                      )
                                      (br $do-once61)
                                    )
                                  )
                                  (call $_abort)
                                )
                                (block
                                  (i32.store
                                    (i32.const 5544)
                                    (i32.or
                                      (get_local $2)
                                      (get_local $1)
                                    )
                                  )
                                  (set_local $12
                                    (get_local $0)
                                  )
                                  (set_local $19
                                    (i32.add
                                      (get_local $0)
                                      (i32.const 8)
                                    )
                                  )
                                )
                              )
                            )
                            (i32.store
                              (get_local $19)
                              (get_local $7)
                            )
                            (i32.store offset=12
                              (get_local $12)
                              (get_local $7)
                            )
                            (i32.store offset=8
                              (get_local $7)
                              (get_local $12)
                            )
                            (i32.store offset=12
                              (get_local $7)
                              (get_local $0)
                            )
                            (br $do-once46)
                          )
                        )
                        (set_local $0
                          (i32.add
                            (i32.shl
                              (tee_local $1
                                (block $do-once63 i32
                                  (if i32
                                    (tee_local $0
                                      (i32.shr_u
                                        (get_local $5)
                                        (i32.const 8)
                                      )
                                    )
                                    (block i32
                                      (drop
                                        (br_if $do-once63
                                          (i32.const 31)
                                          (i32.gt_u
                                            (get_local $5)
                                            (i32.const 16777215)
                                          )
                                        )
                                      )
                                      (i32.or
                                        (i32.and
                                          (i32.shr_u
                                            (get_local $5)
                                            (i32.add
                                              (tee_local $0
                                                (i32.add
                                                  (i32.sub
                                                    (i32.const 14)
                                                    (i32.or
                                                      (i32.or
                                                        (tee_local $2
                                                          (i32.and
                                                            (i32.shr_u
                                                              (i32.add
                                                                (tee_local $1
                                                                  (i32.shl
                                                                    (get_local $0)
                                                                    (tee_local $0
                                                                      (i32.and
                                                                        (i32.shr_u
                                                                          (i32.add
                                                                            (get_local $0)
                                                                            (i32.const 1048320)
                                                                          )
                                                                          (i32.const 16)
                                                                        )
                                                                        (i32.const 8)
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                (i32.const 520192)
                                                              )
                                                              (i32.const 16)
                                                            )
                                                            (i32.const 4)
                                                          )
                                                        )
                                                        (get_local $0)
                                                      )
                                                      (tee_local $1
                                                        (i32.and
                                                          (i32.shr_u
                                                            (i32.add
                                                              (tee_local $0
                                                                (i32.shl
                                                                  (get_local $1)
                                                                  (get_local $2)
                                                                )
                                                              )
                                                              (i32.const 245760)
                                                            )
                                                            (i32.const 16)
                                                          )
                                                          (i32.const 2)
                                                        )
                                                      )
                                                    )
                                                  )
                                                  (i32.shr_u
                                                    (i32.shl
                                                      (get_local $0)
                                                      (get_local $1)
                                                    )
                                                    (i32.const 15)
                                                  )
                                                )
                                              )
                                              (i32.const 7)
                                            )
                                          )
                                          (i32.const 1)
                                        )
                                        (i32.shl
                                          (get_local $0)
                                          (i32.const 1)
                                        )
                                      )
                                    )
                                    (i32.const 0)
                                  )
                                )
                              )
                              (i32.const 2)
                            )
                            (i32.const 5848)
                          )
                        )
                        (i32.store offset=28
                          (get_local $7)
                          (get_local $1)
                        )
                        (i32.store offset=4
                          (tee_local $2
                            (i32.add
                              (get_local $7)
                              (i32.const 16)
                            )
                          )
                          (i32.const 0)
                        )
                        (i32.store
                          (get_local $2)
                          (i32.const 0)
                        )
                        (if
                          (i32.eqz
                            (i32.and
                              (tee_local $2
                                (i32.load
                                  (i32.const 5548)
                                )
                              )
                              (tee_local $3
                                (i32.shl
                                  (i32.const 1)
                                  (get_local $1)
                                )
                              )
                            )
                          )
                          (block
                            (i32.store
                              (i32.const 5548)
                              (i32.or
                                (get_local $2)
                                (get_local $3)
                              )
                            )
                            (i32.store
                              (get_local $0)
                              (get_local $7)
                            )
                            (i32.store offset=24
                              (get_local $7)
                              (get_local $0)
                            )
                            (i32.store offset=12
                              (get_local $7)
                              (get_local $7)
                            )
                            (i32.store offset=8
                              (get_local $7)
                              (get_local $7)
                            )
                            (br $do-once46)
                          )
                        )
                        (set_local $0
                          (i32.load
                            (get_local $0)
                          )
                        )
                        (set_local $2
                          (i32.sub
                            (i32.const 25)
                            (i32.shr_u
                              (get_local $1)
                              (i32.const 1)
                            )
                          )
                        )
                        (set_local $1
                          (i32.shl
                            (get_local $5)
                            (if i32
                              (i32.eq
                                (get_local $1)
                                (i32.const 31)
                              )
                              (i32.const 0)
                              (get_local $2)
                            )
                          )
                        )
                        (block $__rjto$7
                          (block $__rjti$7
                            (block $__rjti$6
                              (loop $while-in66
                                (br_if $__rjti$7
                                  (i32.eq
                                    (i32.and
                                      (i32.load offset=4
                                        (get_local $0)
                                      )
                                      (i32.const -8)
                                    )
                                    (get_local $5)
                                  )
                                )
                                (set_local $2
                                  (i32.shl
                                    (get_local $1)
                                    (i32.const 1)
                                  )
                                )
                                (br_if $__rjti$6
                                  (i32.eqz
                                    (tee_local $3
                                      (i32.load
                                        (tee_local $1
                                          (i32.add
                                            (i32.add
                                              (get_local $0)
                                              (i32.const 16)
                                            )
                                            (i32.shl
                                              (i32.shr_u
                                                (get_local $1)
                                                (i32.const 31)
                                              )
                                              (i32.const 2)
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                                (set_local $1
                                  (get_local $2)
                                )
                                (set_local $0
                                  (get_local $3)
                                )
                                (br $while-in66)
                              )
                            )
                            (if
                              (i32.lt_u
                                (get_local $1)
                                (i32.load
                                  (i32.const 5560)
                                )
                              )
                              (call $_abort)
                              (block
                                (i32.store
                                  (get_local $1)
                                  (get_local $7)
                                )
                                (i32.store offset=24
                                  (get_local $7)
                                  (get_local $0)
                                )
                                (i32.store offset=12
                                  (get_local $7)
                                  (get_local $7)
                                )
                                (i32.store offset=8
                                  (get_local $7)
                                  (get_local $7)
                                )
                                (br $do-once46)
                              )
                            )
                            (br $__rjto$7)
                          )
                          (if
                            (i32.and
                              (i32.ge_u
                                (tee_local $1
                                  (i32.load
                                    (tee_local $2
                                      (i32.add
                                        (get_local $0)
                                        (i32.const 8)
                                      )
                                    )
                                  )
                                )
                                (tee_local $3
                                  (i32.load
                                    (i32.const 5560)
                                  )
                                )
                              )
                              (i32.ge_u
                                (get_local $0)
                                (get_local $3)
                              )
                            )
                            (block
                              (i32.store offset=12
                                (get_local $1)
                                (get_local $7)
                              )
                              (i32.store
                                (get_local $2)
                                (get_local $7)
                              )
                              (i32.store offset=8
                                (get_local $7)
                                (get_local $1)
                              )
                              (i32.store offset=12
                                (get_local $7)
                                (get_local $0)
                              )
                              (i32.store offset=24
                                (get_local $7)
                                (i32.const 0)
                              )
                            )
                            (call $_abort)
                          )
                        )
                      )
                    )
                  )
                  (set_global $STACKTOP
                    (get_local $13)
                  )
                  (return
                    (i32.add
                      (get_local $9)
                      (i32.const 8)
                    )
                  )
                )
              )
            )
            (loop $while-in68
              (block $while-out67
                (if
                  (i32.le_u
                    (tee_local $5
                      (i32.load
                        (get_local $3)
                      )
                    )
                    (get_local $6)
                  )
                  (br_if $while-out67
                    (i32.gt_u
                      (tee_local $10
                        (i32.add
                          (get_local $5)
                          (i32.load offset=4
                            (get_local $3)
                          )
                        )
                      )
                      (get_local $6)
                    )
                  )
                )
                (set_local $3
                  (i32.load offset=8
                    (get_local $3)
                  )
                )
                (br $while-in68)
              )
            )
            (set_local $4
              (i32.and
                (i32.sub
                  (i32.const 0)
                  (tee_local $5
                    (i32.add
                      (tee_local $3
                        (i32.add
                          (get_local $10)
                          (i32.const -47)
                        )
                      )
                      (i32.const 8)
                    )
                  )
                )
                (i32.const 7)
              )
            )
            (set_local $8
              (i32.add
                (if i32
                  (i32.lt_u
                    (tee_local $3
                      (i32.add
                        (get_local $3)
                        (if i32
                          (i32.and
                            (get_local $5)
                            (i32.const 7)
                          )
                          (get_local $4)
                          (i32.const 0)
                        )
                      )
                    )
                    (tee_local $12
                      (i32.add
                        (get_local $6)
                        (i32.const 16)
                      )
                    )
                  )
                  (tee_local $3
                    (get_local $6)
                  )
                  (get_local $3)
                )
                (i32.const 8)
              )
            )
            (set_local $5
              (i32.add
                (get_local $3)
                (i32.const 24)
              )
            )
            (set_local $11
              (i32.add
                (get_local $2)
                (i32.const -40)
              )
            )
            (set_local $4
              (i32.and
                (i32.sub
                  (i32.const 0)
                  (tee_local $7
                    (i32.add
                      (get_local $1)
                      (i32.const 8)
                    )
                  )
                )
                (i32.const 7)
              )
            )
            (i32.store
              (i32.const 5568)
              (tee_local $7
                (i32.add
                  (get_local $1)
                  (if i32
                    (i32.and
                      (get_local $7)
                      (i32.const 7)
                    )
                    (get_local $4)
                    (tee_local $4
                      (i32.const 0)
                    )
                  )
                )
              )
            )
            (i32.store
              (i32.const 5556)
              (tee_local $4
                (i32.sub
                  (get_local $11)
                  (get_local $4)
                )
              )
            )
            (i32.store offset=4
              (get_local $7)
              (i32.or
                (get_local $4)
                (i32.const 1)
              )
            )
            (i32.store offset=4
              (i32.add
                (get_local $7)
                (get_local $4)
              )
              (i32.const 40)
            )
            (i32.store
              (i32.const 5572)
              (i32.load
                (i32.const 6032)
              )
            )
            (i32.store
              (tee_local $4
                (i32.add
                  (get_local $3)
                  (i32.const 4)
                )
              )
              (i32.const 27)
            )
            (i64.store align=4
              (get_local $8)
              (i64.load align=4
                (i32.const 5992)
              )
            )
            (i64.store offset=8 align=4
              (get_local $8)
              (i64.load align=4
                (i32.const 6000)
              )
            )
            (i32.store
              (i32.const 5992)
              (get_local $1)
            )
            (i32.store
              (i32.const 5996)
              (get_local $2)
            )
            (i32.store
              (i32.const 6004)
              (i32.const 0)
            )
            (i32.store
              (i32.const 6000)
              (get_local $8)
            )
            (set_local $1
              (get_local $5)
            )
            (loop $while-in70
              (i32.store
                (tee_local $1
                  (i32.add
                    (get_local $1)
                    (i32.const 4)
                  )
                )
                (i32.const 7)
              )
              (br_if $while-in70
                (i32.lt_u
                  (i32.add
                    (get_local $1)
                    (i32.const 4)
                  )
                  (get_local $10)
                )
              )
            )
            (if
              (i32.ne
                (get_local $3)
                (get_local $6)
              )
              (block
                (i32.store
                  (get_local $4)
                  (i32.and
                    (i32.load
                      (get_local $4)
                    )
                    (i32.const -2)
                  )
                )
                (i32.store offset=4
                  (get_local $6)
                  (i32.or
                    (tee_local $4
                      (i32.sub
                        (get_local $3)
                        (get_local $6)
                      )
                    )
                    (i32.const 1)
                  )
                )
                (i32.store
                  (get_local $3)
                  (get_local $4)
                )
                (set_local $2
                  (i32.shr_u
                    (get_local $4)
                    (i32.const 3)
                  )
                )
                (if
                  (i32.lt_u
                    (get_local $4)
                    (i32.const 256)
                  )
                  (block
                    (set_local $1
                      (i32.add
                        (i32.shl
                          (i32.shl
                            (get_local $2)
                            (i32.const 1)
                          )
                          (i32.const 2)
                        )
                        (i32.const 5584)
                      )
                    )
                    (if
                      (i32.and
                        (tee_local $3
                          (i32.load
                            (i32.const 5544)
                          )
                        )
                        (tee_local $2
                          (i32.shl
                            (i32.const 1)
                            (get_local $2)
                          )
                        )
                      )
                      (if
                        (i32.lt_u
                          (tee_local $3
                            (i32.load
                              (tee_local $2
                                (i32.add
                                  (get_local $1)
                                  (i32.const 8)
                                )
                              )
                            )
                          )
                          (i32.load
                            (i32.const 5560)
                          )
                        )
                        (call $_abort)
                        (block
                          (set_local $9
                            (get_local $3)
                          )
                          (set_local $20
                            (get_local $2)
                          )
                        )
                      )
                      (block
                        (i32.store
                          (i32.const 5544)
                          (i32.or
                            (get_local $3)
                            (get_local $2)
                          )
                        )
                        (set_local $9
                          (get_local $1)
                        )
                        (set_local $20
                          (i32.add
                            (get_local $1)
                            (i32.const 8)
                          )
                        )
                      )
                    )
                    (i32.store
                      (get_local $20)
                      (get_local $6)
                    )
                    (i32.store offset=12
                      (get_local $9)
                      (get_local $6)
                    )
                    (i32.store offset=8
                      (get_local $6)
                      (get_local $9)
                    )
                    (i32.store offset=12
                      (get_local $6)
                      (get_local $1)
                    )
                    (br $do-once38)
                  )
                )
                (set_local $1
                  (i32.add
                    (i32.shl
                      (tee_local $2
                        (if i32
                          (tee_local $1
                            (i32.shr_u
                              (get_local $4)
                              (i32.const 8)
                            )
                          )
                          (if i32
                            (i32.gt_u
                              (get_local $4)
                              (i32.const 16777215)
                            )
                            (i32.const 31)
                            (i32.or
                              (i32.and
                                (i32.shr_u
                                  (get_local $4)
                                  (i32.add
                                    (tee_local $1
                                      (i32.add
                                        (i32.sub
                                          (i32.const 14)
                                          (i32.or
                                            (i32.or
                                              (tee_local $3
                                                (i32.and
                                                  (i32.shr_u
                                                    (i32.add
                                                      (tee_local $2
                                                        (i32.shl
                                                          (get_local $1)
                                                          (tee_local $1
                                                            (i32.and
                                                              (i32.shr_u
                                                                (i32.add
                                                                  (get_local $1)
                                                                  (i32.const 1048320)
                                                                )
                                                                (i32.const 16)
                                                              )
                                                              (i32.const 8)
                                                            )
                                                          )
                                                        )
                                                      )
                                                      (i32.const 520192)
                                                    )
                                                    (i32.const 16)
                                                  )
                                                  (i32.const 4)
                                                )
                                              )
                                              (get_local $1)
                                            )
                                            (tee_local $2
                                              (i32.and
                                                (i32.shr_u
                                                  (i32.add
                                                    (tee_local $1
                                                      (i32.shl
                                                        (get_local $2)
                                                        (get_local $3)
                                                      )
                                                    )
                                                    (i32.const 245760)
                                                  )
                                                  (i32.const 16)
                                                )
                                                (i32.const 2)
                                              )
                                            )
                                          )
                                        )
                                        (i32.shr_u
                                          (i32.shl
                                            (get_local $1)
                                            (get_local $2)
                                          )
                                          (i32.const 15)
                                        )
                                      )
                                    )
                                    (i32.const 7)
                                  )
                                )
                                (i32.const 1)
                              )
                              (i32.shl
                                (get_local $1)
                                (i32.const 1)
                              )
                            )
                          )
                          (i32.const 0)
                        )
                      )
                      (i32.const 2)
                    )
                    (i32.const 5848)
                  )
                )
                (i32.store offset=28
                  (get_local $6)
                  (get_local $2)
                )
                (i32.store offset=20
                  (get_local $6)
                  (i32.const 0)
                )
                (i32.store
                  (get_local $12)
                  (i32.const 0)
                )
                (if
                  (i32.eqz
                    (i32.and
                      (tee_local $3
                        (i32.load
                          (i32.const 5548)
                        )
                      )
                      (tee_local $5
                        (i32.shl
                          (i32.const 1)
                          (get_local $2)
                        )
                      )
                    )
                  )
                  (block
                    (i32.store
                      (i32.const 5548)
                      (i32.or
                        (get_local $3)
                        (get_local $5)
                      )
                    )
                    (i32.store
                      (get_local $1)
                      (get_local $6)
                    )
                    (i32.store offset=24
                      (get_local $6)
                      (get_local $1)
                    )
                    (i32.store offset=12
                      (get_local $6)
                      (get_local $6)
                    )
                    (i32.store offset=8
                      (get_local $6)
                      (get_local $6)
                    )
                    (br $do-once38)
                  )
                )
                (set_local $1
                  (i32.load
                    (get_local $1)
                  )
                )
                (set_local $3
                  (i32.sub
                    (i32.const 25)
                    (i32.shr_u
                      (get_local $2)
                      (i32.const 1)
                    )
                  )
                )
                (set_local $2
                  (i32.shl
                    (get_local $4)
                    (if i32
                      (i32.eq
                        (get_local $2)
                        (i32.const 31)
                      )
                      (i32.const 0)
                      (get_local $3)
                    )
                  )
                )
                (block $__rjto$9
                  (block $__rjti$9
                    (block $__rjti$8
                      (loop $while-in72
                        (br_if $__rjti$9
                          (i32.eq
                            (i32.and
                              (i32.load offset=4
                                (get_local $1)
                              )
                              (i32.const -8)
                            )
                            (get_local $4)
                          )
                        )
                        (set_local $3
                          (i32.shl
                            (get_local $2)
                            (i32.const 1)
                          )
                        )
                        (br_if $__rjti$8
                          (i32.eqz
                            (tee_local $5
                              (i32.load
                                (tee_local $2
                                  (i32.add
                                    (i32.add
                                      (get_local $1)
                                      (i32.const 16)
                                    )
                                    (i32.shl
                                      (i32.shr_u
                                        (get_local $2)
                                        (i32.const 31)
                                      )
                                      (i32.const 2)
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                        (set_local $2
                          (get_local $3)
                        )
                        (set_local $1
                          (get_local $5)
                        )
                        (br $while-in72)
                      )
                    )
                    (if
                      (i32.lt_u
                        (get_local $2)
                        (i32.load
                          (i32.const 5560)
                        )
                      )
                      (call $_abort)
                      (block
                        (i32.store
                          (get_local $2)
                          (get_local $6)
                        )
                        (i32.store offset=24
                          (get_local $6)
                          (get_local $1)
                        )
                        (i32.store offset=12
                          (get_local $6)
                          (get_local $6)
                        )
                        (i32.store offset=8
                          (get_local $6)
                          (get_local $6)
                        )
                        (br $do-once38)
                      )
                    )
                    (br $__rjto$9)
                  )
                  (if
                    (i32.and
                      (i32.ge_u
                        (tee_local $2
                          (i32.load
                            (tee_local $3
                              (i32.add
                                (get_local $1)
                                (i32.const 8)
                              )
                            )
                          )
                        )
                        (tee_local $5
                          (i32.load
                            (i32.const 5560)
                          )
                        )
                      )
                      (i32.ge_u
                        (get_local $1)
                        (get_local $5)
                      )
                    )
                    (block
                      (i32.store offset=12
                        (get_local $2)
                        (get_local $6)
                      )
                      (i32.store
                        (get_local $3)
                        (get_local $6)
                      )
                      (i32.store offset=8
                        (get_local $6)
                        (get_local $2)
                      )
                      (i32.store offset=12
                        (get_local $6)
                        (get_local $1)
                      )
                      (i32.store offset=24
                        (get_local $6)
                        (i32.const 0)
                      )
                    )
                    (call $_abort)
                  )
                )
              )
            )
          )
          (block
            (if
              (i32.or
                (i32.eqz
                  (tee_local $3
                    (i32.load
                      (i32.const 5560)
                    )
                  )
                )
                (i32.lt_u
                  (get_local $1)
                  (get_local $3)
                )
              )
              (i32.store
                (i32.const 5560)
                (get_local $1)
              )
            )
            (i32.store
              (i32.const 5992)
              (get_local $1)
            )
            (i32.store
              (i32.const 5996)
              (get_local $2)
            )
            (i32.store
              (i32.const 6004)
              (i32.const 0)
            )
            (i32.store
              (i32.const 5580)
              (i32.load
                (i32.const 6016)
              )
            )
            (i32.store
              (i32.const 5576)
              (i32.const -1)
            )
            (set_local $3
              (i32.const 0)
            )
            (loop $while-in41
              (i32.store offset=12
                (tee_local $5
                  (i32.add
                    (i32.shl
                      (i32.shl
                        (get_local $3)
                        (i32.const 1)
                      )
                      (i32.const 2)
                    )
                    (i32.const 5584)
                  )
                )
                (get_local $5)
              )
              (i32.store offset=8
                (get_local $5)
                (get_local $5)
              )
              (br_if $while-in41
                (i32.ne
                  (tee_local $3
                    (i32.add
                      (get_local $3)
                      (i32.const 1)
                    )
                  )
                  (i32.const 32)
                )
              )
            )
            (set_local $3
              (i32.add
                (get_local $2)
                (i32.const -40)
              )
            )
            (set_local $2
              (i32.and
                (i32.sub
                  (i32.const 0)
                  (tee_local $5
                    (i32.add
                      (get_local $1)
                      (i32.const 8)
                    )
                  )
                )
                (i32.const 7)
              )
            )
            (i32.store
              (i32.const 5568)
              (tee_local $1
                (i32.add
                  (get_local $1)
                  (if i32
                    (i32.and
                      (get_local $5)
                      (i32.const 7)
                    )
                    (get_local $2)
                    (tee_local $2
                      (i32.const 0)
                    )
                  )
                )
              )
            )
            (i32.store
              (i32.const 5556)
              (tee_local $2
                (i32.sub
                  (get_local $3)
                  (get_local $2)
                )
              )
            )
            (i32.store offset=4
              (get_local $1)
              (i32.or
                (get_local $2)
                (i32.const 1)
              )
            )
            (i32.store offset=4
              (i32.add
                (get_local $1)
                (get_local $2)
              )
              (i32.const 40)
            )
            (i32.store
              (i32.const 5572)
              (i32.load
                (i32.const 6032)
              )
            )
          )
        )
      )
      (if
        (i32.gt_u
          (tee_local $1
            (i32.load
              (i32.const 5556)
            )
          )
          (get_local $0)
        )
        (block
          (i32.store
            (i32.const 5556)
            (tee_local $2
              (i32.sub
                (get_local $1)
                (get_local $0)
              )
            )
          )
          (i32.store
            (i32.const 5568)
            (tee_local $3
              (i32.add
                (tee_local $1
                  (i32.load
                    (i32.const 5568)
                  )
                )
                (get_local $0)
              )
            )
          )
          (i32.store offset=4
            (get_local $3)
            (i32.or
              (get_local $2)
              (i32.const 1)
            )
          )
          (i32.store offset=4
            (get_local $1)
            (i32.or
              (get_local $0)
              (i32.const 3)
            )
          )
          (set_global $STACKTOP
            (get_local $13)
          )
          (return
            (i32.add
              (get_local $1)
              (i32.const 8)
            )
          )
        )
      )
    )
    (i32.store
      (call $___errno_location)
      (i32.const 12)
    )
    (set_global $STACKTOP
      (get_local $13)
    )
    (i32.const 0)
  )
  (func $_free (param $0 i32)
    (local $1 i32)
    (local $2 i32)
    (local $3 i32)
    (local $4 i32)
    (local $5 i32)
    (local $6 i32)
    (local $7 i32)
    (local $8 i32)
    (local $9 i32)
    (local $10 i32)
    (local $11 i32)
    (local $12 i32)
    (local $13 i32)
    (local $14 i32)
    (local $15 i32)
    (if
      (i32.eqz
        (get_local $0)
      )
      (return)
    )
    (if
      (i32.lt_u
        (tee_local $1
          (i32.add
            (get_local $0)
            (i32.const -8)
          )
        )
        (tee_local $11
          (i32.load
            (i32.const 5560)
          )
        )
      )
      (call $_abort)
    )
    (if
      (i32.eq
        (tee_local $10
          (i32.and
            (tee_local $0
              (i32.load
                (i32.add
                  (get_local $0)
                  (i32.const -4)
                )
              )
            )
            (i32.const 3)
          )
        )
        (i32.const 1)
      )
      (call $_abort)
    )
    (set_local $6
      (i32.add
        (get_local $1)
        (tee_local $4
          (i32.and
            (get_local $0)
            (i32.const -8)
          )
        )
      )
    )
    (block $do-once
      (if
        (i32.and
          (get_local $0)
          (i32.const 1)
        )
        (block
          (set_local $3
            (get_local $1)
          )
          (set_local $2
            (get_local $4)
          )
        )
        (block
          (set_local $8
            (i32.load
              (get_local $1)
            )
          )
          (if
            (i32.eqz
              (get_local $10)
            )
            (return)
          )
          (if
            (i32.lt_u
              (tee_local $0
                (i32.add
                  (get_local $1)
                  (i32.sub
                    (i32.const 0)
                    (get_local $8)
                  )
                )
              )
              (get_local $11)
            )
            (call $_abort)
          )
          (set_local $1
            (i32.add
              (get_local $8)
              (get_local $4)
            )
          )
          (if
            (i32.eq
              (get_local $0)
              (i32.load
                (i32.const 5564)
              )
            )
            (block
              (if
                (i32.ne
                  (i32.and
                    (tee_local $3
                      (i32.load
                        (tee_local $2
                          (i32.add
                            (get_local $6)
                            (i32.const 4)
                          )
                        )
                      )
                    )
                    (i32.const 3)
                  )
                  (i32.const 3)
                )
                (block
                  (set_local $3
                    (get_local $0)
                  )
                  (set_local $2
                    (get_local $1)
                  )
                  (br $do-once)
                )
              )
              (i32.store
                (i32.const 5552)
                (get_local $1)
              )
              (i32.store
                (get_local $2)
                (i32.and
                  (get_local $3)
                  (i32.const -2)
                )
              )
              (i32.store offset=4
                (get_local $0)
                (i32.or
                  (get_local $1)
                  (i32.const 1)
                )
              )
              (i32.store
                (i32.add
                  (get_local $0)
                  (get_local $1)
                )
                (get_local $1)
              )
              (return)
            )
          )
          (set_local $10
            (i32.shr_u
              (get_local $8)
              (i32.const 3)
            )
          )
          (if
            (i32.lt_u
              (get_local $8)
              (i32.const 256)
            )
            (block
              (set_local $3
                (i32.load offset=12
                  (get_local $0)
                )
              )
              (if
                (i32.ne
                  (tee_local $4
                    (i32.load offset=8
                      (get_local $0)
                    )
                  )
                  (tee_local $2
                    (i32.add
                      (i32.shl
                        (i32.shl
                          (get_local $10)
                          (i32.const 1)
                        )
                        (i32.const 2)
                      )
                      (i32.const 5584)
                    )
                  )
                )
                (block
                  (if
                    (i32.lt_u
                      (get_local $4)
                      (get_local $11)
                    )
                    (call $_abort)
                  )
                  (if
                    (i32.ne
                      (i32.load offset=12
                        (get_local $4)
                      )
                      (get_local $0)
                    )
                    (call $_abort)
                  )
                )
              )
              (if
                (i32.eq
                  (get_local $3)
                  (get_local $4)
                )
                (block
                  (i32.store
                    (i32.const 5544)
                    (i32.and
                      (i32.load
                        (i32.const 5544)
                      )
                      (i32.xor
                        (i32.shl
                          (i32.const 1)
                          (get_local $10)
                        )
                        (i32.const -1)
                      )
                    )
                  )
                  (set_local $3
                    (get_local $0)
                  )
                  (set_local $2
                    (get_local $1)
                  )
                  (br $do-once)
                )
              )
              (if
                (i32.eq
                  (get_local $3)
                  (get_local $2)
                )
                (set_local $5
                  (i32.add
                    (get_local $3)
                    (i32.const 8)
                  )
                )
                (block
                  (if
                    (i32.lt_u
                      (get_local $3)
                      (get_local $11)
                    )
                    (call $_abort)
                  )
                  (if
                    (i32.eq
                      (i32.load
                        (tee_local $2
                          (i32.add
                            (get_local $3)
                            (i32.const 8)
                          )
                        )
                      )
                      (get_local $0)
                    )
                    (set_local $5
                      (get_local $2)
                    )
                    (call $_abort)
                  )
                )
              )
              (i32.store offset=12
                (get_local $4)
                (get_local $3)
              )
              (i32.store
                (get_local $5)
                (get_local $4)
              )
              (set_local $3
                (get_local $0)
              )
              (set_local $2
                (get_local $1)
              )
              (br $do-once)
            )
          )
          (set_local $12
            (i32.load offset=24
              (get_local $0)
            )
          )
          (block $do-once0
            (if
              (i32.eq
                (tee_local $4
                  (i32.load offset=12
                    (get_local $0)
                  )
                )
                (get_local $0)
              )
              (block
                (if
                  (tee_local $4
                    (i32.load
                      (tee_local $8
                        (i32.add
                          (tee_local $5
                            (i32.add
                              (get_local $0)
                              (i32.const 16)
                            )
                          )
                          (i32.const 4)
                        )
                      )
                    )
                  )
                  (set_local $5
                    (get_local $8)
                  )
                  (if
                    (i32.eqz
                      (tee_local $4
                        (i32.load
                          (get_local $5)
                        )
                      )
                    )
                    (block
                      (set_local $7
                        (i32.const 0)
                      )
                      (br $do-once0)
                    )
                  )
                )
                (loop $while-in
                  (if
                    (tee_local $10
                      (i32.load
                        (tee_local $8
                          (i32.add
                            (get_local $4)
                            (i32.const 20)
                          )
                        )
                      )
                    )
                    (block
                      (set_local $4
                        (get_local $10)
                      )
                      (set_local $5
                        (get_local $8)
                      )
                      (br $while-in)
                    )
                  )
                  (if
                    (tee_local $10
                      (i32.load
                        (tee_local $8
                          (i32.add
                            (get_local $4)
                            (i32.const 16)
                          )
                        )
                      )
                    )
                    (block
                      (set_local $4
                        (get_local $10)
                      )
                      (set_local $5
                        (get_local $8)
                      )
                      (br $while-in)
                    )
                  )
                )
                (if
                  (i32.lt_u
                    (get_local $5)
                    (get_local $11)
                  )
                  (call $_abort)
                  (block
                    (i32.store
                      (get_local $5)
                      (i32.const 0)
                    )
                    (set_local $7
                      (get_local $4)
                    )
                  )
                )
              )
              (block
                (if
                  (i32.lt_u
                    (tee_local $5
                      (i32.load offset=8
                        (get_local $0)
                      )
                    )
                    (get_local $11)
                  )
                  (call $_abort)
                )
                (if
                  (i32.ne
                    (i32.load
                      (tee_local $8
                        (i32.add
                          (get_local $5)
                          (i32.const 12)
                        )
                      )
                    )
                    (get_local $0)
                  )
                  (call $_abort)
                )
                (if
                  (i32.eq
                    (i32.load
                      (tee_local $10
                        (i32.add
                          (get_local $4)
                          (i32.const 8)
                        )
                      )
                    )
                    (get_local $0)
                  )
                  (block
                    (i32.store
                      (get_local $8)
                      (get_local $4)
                    )
                    (i32.store
                      (get_local $10)
                      (get_local $5)
                    )
                    (set_local $7
                      (get_local $4)
                    )
                  )
                  (call $_abort)
                )
              )
            )
          )
          (if
            (get_local $12)
            (block
              (if
                (i32.eq
                  (get_local $0)
                  (i32.load
                    (tee_local $5
                      (i32.add
                        (i32.shl
                          (tee_local $4
                            (i32.load offset=28
                              (get_local $0)
                            )
                          )
                          (i32.const 2)
                        )
                        (i32.const 5848)
                      )
                    )
                  )
                )
                (block
                  (i32.store
                    (get_local $5)
                    (get_local $7)
                  )
                  (if
                    (i32.eqz
                      (get_local $7)
                    )
                    (block
                      (i32.store
                        (i32.const 5548)
                        (i32.and
                          (i32.load
                            (i32.const 5548)
                          )
                          (i32.xor
                            (i32.shl
                              (i32.const 1)
                              (get_local $4)
                            )
                            (i32.const -1)
                          )
                        )
                      )
                      (set_local $3
                        (get_local $0)
                      )
                      (set_local $2
                        (get_local $1)
                      )
                      (br $do-once)
                    )
                  )
                )
                (block
                  (if
                    (i32.lt_u
                      (get_local $12)
                      (i32.load
                        (i32.const 5560)
                      )
                    )
                    (call $_abort)
                  )
                  (if
                    (i32.eq
                      (i32.load
                        (tee_local $4
                          (i32.add
                            (get_local $12)
                            (i32.const 16)
                          )
                        )
                      )
                      (get_local $0)
                    )
                    (i32.store
                      (get_local $4)
                      (get_local $7)
                    )
                    (i32.store offset=20
                      (get_local $12)
                      (get_local $7)
                    )
                  )
                  (if
                    (i32.eqz
                      (get_local $7)
                    )
                    (block
                      (set_local $3
                        (get_local $0)
                      )
                      (set_local $2
                        (get_local $1)
                      )
                      (br $do-once)
                    )
                  )
                )
              )
              (if
                (i32.lt_u
                  (get_local $7)
                  (tee_local $5
                    (i32.load
                      (i32.const 5560)
                    )
                  )
                )
                (call $_abort)
              )
              (i32.store offset=24
                (get_local $7)
                (get_local $12)
              )
              (if
                (tee_local $4
                  (i32.load
                    (tee_local $8
                      (i32.add
                        (get_local $0)
                        (i32.const 16)
                      )
                    )
                  )
                )
                (if
                  (i32.lt_u
                    (get_local $4)
                    (get_local $5)
                  )
                  (call $_abort)
                  (block
                    (i32.store offset=16
                      (get_local $7)
                      (get_local $4)
                    )
                    (i32.store offset=24
                      (get_local $4)
                      (get_local $7)
                    )
                  )
                )
              )
              (if
                (tee_local $4
                  (i32.load offset=4
                    (get_local $8)
                  )
                )
                (if
                  (i32.lt_u
                    (get_local $4)
                    (i32.load
                      (i32.const 5560)
                    )
                  )
                  (call $_abort)
                  (block
                    (i32.store offset=20
                      (get_local $7)
                      (get_local $4)
                    )
                    (i32.store offset=24
                      (get_local $4)
                      (get_local $7)
                    )
                    (set_local $3
                      (get_local $0)
                    )
                    (set_local $2
                      (get_local $1)
                    )
                  )
                )
                (block
                  (set_local $3
                    (get_local $0)
                  )
                  (set_local $2
                    (get_local $1)
                  )
                )
              )
            )
            (block
              (set_local $3
                (get_local $0)
              )
              (set_local $2
                (get_local $1)
              )
            )
          )
        )
      )
    )
    (if
      (i32.ge_u
        (get_local $3)
        (get_local $6)
      )
      (call $_abort)
    )
    (if
      (i32.eqz
        (i32.and
          (tee_local $0
            (i32.load
              (tee_local $1
                (i32.add
                  (get_local $6)
                  (i32.const 4)
                )
              )
            )
          )
          (i32.const 1)
        )
      )
      (call $_abort)
    )
    (if
      (i32.and
        (get_local $0)
        (i32.const 2)
      )
      (block
        (i32.store
          (get_local $1)
          (i32.and
            (get_local $0)
            (i32.const -2)
          )
        )
        (i32.store offset=4
          (get_local $3)
          (i32.or
            (get_local $2)
            (i32.const 1)
          )
        )
        (i32.store
          (i32.add
            (get_local $3)
            (get_local $2)
          )
          (get_local $2)
        )
      )
      (block
        (if
          (i32.eq
            (get_local $6)
            (i32.load
              (i32.const 5568)
            )
          )
          (block
            (i32.store
              (i32.const 5556)
              (tee_local $0
                (i32.add
                  (i32.load
                    (i32.const 5556)
                  )
                  (get_local $2)
                )
              )
            )
            (i32.store
              (i32.const 5568)
              (get_local $3)
            )
            (i32.store offset=4
              (get_local $3)
              (i32.or
                (get_local $0)
                (i32.const 1)
              )
            )
            (if
              (i32.ne
                (get_local $3)
                (i32.load
                  (i32.const 5564)
                )
              )
              (return)
            )
            (i32.store
              (i32.const 5564)
              (i32.const 0)
            )
            (i32.store
              (i32.const 5552)
              (i32.const 0)
            )
            (return)
          )
        )
        (if
          (i32.eq
            (get_local $6)
            (i32.load
              (i32.const 5564)
            )
          )
          (block
            (i32.store
              (i32.const 5552)
              (tee_local $0
                (i32.add
                  (i32.load
                    (i32.const 5552)
                  )
                  (get_local $2)
                )
              )
            )
            (i32.store
              (i32.const 5564)
              (get_local $3)
            )
            (i32.store offset=4
              (get_local $3)
              (i32.or
                (get_local $0)
                (i32.const 1)
              )
            )
            (i32.store
              (i32.add
                (get_local $3)
                (get_local $0)
              )
              (get_local $0)
            )
            (return)
          )
        )
        (set_local $5
          (i32.add
            (i32.and
              (get_local $0)
              (i32.const -8)
            )
            (get_local $2)
          )
        )
        (set_local $4
          (i32.shr_u
            (get_local $0)
            (i32.const 3)
          )
        )
        (block $do-once4
          (if
            (i32.lt_u
              (get_local $0)
              (i32.const 256)
            )
            (block
              (set_local $2
                (i32.load offset=12
                  (get_local $6)
                )
              )
              (if
                (i32.ne
                  (tee_local $1
                    (i32.load offset=8
                      (get_local $6)
                    )
                  )
                  (tee_local $0
                    (i32.add
                      (i32.shl
                        (i32.shl
                          (get_local $4)
                          (i32.const 1)
                        )
                        (i32.const 2)
                      )
                      (i32.const 5584)
                    )
                  )
                )
                (block
                  (if
                    (i32.lt_u
                      (get_local $1)
                      (i32.load
                        (i32.const 5560)
                      )
                    )
                    (call $_abort)
                  )
                  (if
                    (i32.ne
                      (i32.load offset=12
                        (get_local $1)
                      )
                      (get_local $6)
                    )
                    (call $_abort)
                  )
                )
              )
              (if
                (i32.eq
                  (get_local $2)
                  (get_local $1)
                )
                (block
                  (i32.store
                    (i32.const 5544)
                    (i32.and
                      (i32.load
                        (i32.const 5544)
                      )
                      (i32.xor
                        (i32.shl
                          (i32.const 1)
                          (get_local $4)
                        )
                        (i32.const -1)
                      )
                    )
                  )
                  (br $do-once4)
                )
              )
              (if
                (i32.eq
                  (get_local $2)
                  (get_local $0)
                )
                (set_local $14
                  (i32.add
                    (get_local $2)
                    (i32.const 8)
                  )
                )
                (block
                  (if
                    (i32.lt_u
                      (get_local $2)
                      (i32.load
                        (i32.const 5560)
                      )
                    )
                    (call $_abort)
                  )
                  (if
                    (i32.eq
                      (i32.load
                        (tee_local $0
                          (i32.add
                            (get_local $2)
                            (i32.const 8)
                          )
                        )
                      )
                      (get_local $6)
                    )
                    (set_local $14
                      (get_local $0)
                    )
                    (call $_abort)
                  )
                )
              )
              (i32.store offset=12
                (get_local $1)
                (get_local $2)
              )
              (i32.store
                (get_local $14)
                (get_local $1)
              )
            )
            (block
              (set_local $7
                (i32.load offset=24
                  (get_local $6)
                )
              )
              (block $do-once6
                (if
                  (i32.eq
                    (tee_local $0
                      (i32.load offset=12
                        (get_local $6)
                      )
                    )
                    (get_local $6)
                  )
                  (block
                    (if
                      (tee_local $0
                        (i32.load
                          (tee_local $1
                            (i32.add
                              (tee_local $2
                                (i32.add
                                  (get_local $6)
                                  (i32.const 16)
                                )
                              )
                              (i32.const 4)
                            )
                          )
                        )
                      )
                      (set_local $2
                        (get_local $1)
                      )
                      (if
                        (i32.eqz
                          (tee_local $0
                            (i32.load
                              (get_local $2)
                            )
                          )
                        )
                        (block
                          (set_local $9
                            (i32.const 0)
                          )
                          (br $do-once6)
                        )
                      )
                    )
                    (loop $while-in9
                      (if
                        (tee_local $4
                          (i32.load
                            (tee_local $1
                              (i32.add
                                (get_local $0)
                                (i32.const 20)
                              )
                            )
                          )
                        )
                        (block
                          (set_local $0
                            (get_local $4)
                          )
                          (set_local $2
                            (get_local $1)
                          )
                          (br $while-in9)
                        )
                      )
                      (if
                        (tee_local $4
                          (i32.load
                            (tee_local $1
                              (i32.add
                                (get_local $0)
                                (i32.const 16)
                              )
                            )
                          )
                        )
                        (block
                          (set_local $0
                            (get_local $4)
                          )
                          (set_local $2
                            (get_local $1)
                          )
                          (br $while-in9)
                        )
                      )
                    )
                    (if
                      (i32.lt_u
                        (get_local $2)
                        (i32.load
                          (i32.const 5560)
                        )
                      )
                      (call $_abort)
                      (block
                        (i32.store
                          (get_local $2)
                          (i32.const 0)
                        )
                        (set_local $9
                          (get_local $0)
                        )
                      )
                    )
                  )
                  (block
                    (if
                      (i32.lt_u
                        (tee_local $2
                          (i32.load offset=8
                            (get_local $6)
                          )
                        )
                        (i32.load
                          (i32.const 5560)
                        )
                      )
                      (call $_abort)
                    )
                    (if
                      (i32.ne
                        (i32.load
                          (tee_local $1
                            (i32.add
                              (get_local $2)
                              (i32.const 12)
                            )
                          )
                        )
                        (get_local $6)
                      )
                      (call $_abort)
                    )
                    (if
                      (i32.eq
                        (i32.load
                          (tee_local $4
                            (i32.add
                              (get_local $0)
                              (i32.const 8)
                            )
                          )
                        )
                        (get_local $6)
                      )
                      (block
                        (i32.store
                          (get_local $1)
                          (get_local $0)
                        )
                        (i32.store
                          (get_local $4)
                          (get_local $2)
                        )
                        (set_local $9
                          (get_local $0)
                        )
                      )
                      (call $_abort)
                    )
                  )
                )
              )
              (if
                (get_local $7)
                (block
                  (if
                    (i32.eq
                      (get_local $6)
                      (i32.load
                        (tee_local $2
                          (i32.add
                            (i32.shl
                              (tee_local $0
                                (i32.load offset=28
                                  (get_local $6)
                                )
                              )
                              (i32.const 2)
                            )
                            (i32.const 5848)
                          )
                        )
                      )
                    )
                    (block
                      (i32.store
                        (get_local $2)
                        (get_local $9)
                      )
                      (if
                        (i32.eqz
                          (get_local $9)
                        )
                        (block
                          (i32.store
                            (i32.const 5548)
                            (i32.and
                              (i32.load
                                (i32.const 5548)
                              )
                              (i32.xor
                                (i32.shl
                                  (i32.const 1)
                                  (get_local $0)
                                )
                                (i32.const -1)
                              )
                            )
                          )
                          (br $do-once4)
                        )
                      )
                    )
                    (block
                      (if
                        (i32.lt_u
                          (get_local $7)
                          (i32.load
                            (i32.const 5560)
                          )
                        )
                        (call $_abort)
                      )
                      (if
                        (i32.eq
                          (i32.load
                            (tee_local $0
                              (i32.add
                                (get_local $7)
                                (i32.const 16)
                              )
                            )
                          )
                          (get_local $6)
                        )
                        (i32.store
                          (get_local $0)
                          (get_local $9)
                        )
                        (i32.store offset=20
                          (get_local $7)
                          (get_local $9)
                        )
                      )
                      (br_if $do-once4
                        (i32.eqz
                          (get_local $9)
                        )
                      )
                    )
                  )
                  (if
                    (i32.lt_u
                      (get_local $9)
                      (tee_local $2
                        (i32.load
                          (i32.const 5560)
                        )
                      )
                    )
                    (call $_abort)
                  )
                  (i32.store offset=24
                    (get_local $9)
                    (get_local $7)
                  )
                  (if
                    (tee_local $0
                      (i32.load
                        (tee_local $1
                          (i32.add
                            (get_local $6)
                            (i32.const 16)
                          )
                        )
                      )
                    )
                    (if
                      (i32.lt_u
                        (get_local $0)
                        (get_local $2)
                      )
                      (call $_abort)
                      (block
                        (i32.store offset=16
                          (get_local $9)
                          (get_local $0)
                        )
                        (i32.store offset=24
                          (get_local $0)
                          (get_local $9)
                        )
                      )
                    )
                  )
                  (if
                    (tee_local $0
                      (i32.load offset=4
                        (get_local $1)
                      )
                    )
                    (if
                      (i32.lt_u
                        (get_local $0)
                        (i32.load
                          (i32.const 5560)
                        )
                      )
                      (call $_abort)
                      (block
                        (i32.store offset=20
                          (get_local $9)
                          (get_local $0)
                        )
                        (i32.store offset=24
                          (get_local $0)
                          (get_local $9)
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
        (i32.store offset=4
          (get_local $3)
          (i32.or
            (get_local $5)
            (i32.const 1)
          )
        )
        (i32.store
          (i32.add
            (get_local $3)
            (get_local $5)
          )
          (get_local $5)
        )
        (if
          (i32.eq
            (get_local $3)
            (i32.load
              (i32.const 5564)
            )
          )
          (block
            (i32.store
              (i32.const 5552)
              (get_local $5)
            )
            (return)
          )
          (set_local $2
            (get_local $5)
          )
        )
      )
    )
    (set_local $1
      (i32.shr_u
        (get_local $2)
        (i32.const 3)
      )
    )
    (if
      (i32.lt_u
        (get_local $2)
        (i32.const 256)
      )
      (block
        (set_local $0
          (i32.add
            (i32.shl
              (i32.shl
                (get_local $1)
                (i32.const 1)
              )
              (i32.const 2)
            )
            (i32.const 5584)
          )
        )
        (if
          (i32.and
            (tee_local $2
              (i32.load
                (i32.const 5544)
              )
            )
            (tee_local $1
              (i32.shl
                (i32.const 1)
                (get_local $1)
              )
            )
          )
          (if
            (i32.lt_u
              (tee_local $1
                (i32.load
                  (tee_local $2
                    (i32.add
                      (get_local $0)
                      (i32.const 8)
                    )
                  )
                )
              )
              (i32.load
                (i32.const 5560)
              )
            )
            (call $_abort)
            (block
              (set_local $13
                (get_local $1)
              )
              (set_local $15
                (get_local $2)
              )
            )
          )
          (block
            (i32.store
              (i32.const 5544)
              (i32.or
                (get_local $2)
                (get_local $1)
              )
            )
            (set_local $13
              (get_local $0)
            )
            (set_local $15
              (i32.add
                (get_local $0)
                (i32.const 8)
              )
            )
          )
        )
        (i32.store
          (get_local $15)
          (get_local $3)
        )
        (i32.store offset=12
          (get_local $13)
          (get_local $3)
        )
        (i32.store offset=8
          (get_local $3)
          (get_local $13)
        )
        (i32.store offset=12
          (get_local $3)
          (get_local $0)
        )
        (return)
      )
    )
    (set_local $0
      (i32.add
        (i32.shl
          (tee_local $1
            (if i32
              (tee_local $0
                (i32.shr_u
                  (get_local $2)
                  (i32.const 8)
                )
              )
              (if i32
                (i32.gt_u
                  (get_local $2)
                  (i32.const 16777215)
                )
                (i32.const 31)
                (i32.or
                  (i32.and
                    (i32.shr_u
                      (get_local $2)
                      (i32.add
                        (tee_local $0
                          (i32.add
                            (i32.sub
                              (i32.const 14)
                              (i32.or
                                (i32.or
                                  (tee_local $4
                                    (i32.and
                                      (i32.shr_u
                                        (i32.add
                                          (tee_local $1
                                            (i32.shl
                                              (get_local $0)
                                              (tee_local $0
                                                (i32.and
                                                  (i32.shr_u
                                                    (i32.add
                                                      (get_local $0)
                                                      (i32.const 1048320)
                                                    )
                                                    (i32.const 16)
                                                  )
                                                  (i32.const 8)
                                                )
                                              )
                                            )
                                          )
                                          (i32.const 520192)
                                        )
                                        (i32.const 16)
                                      )
                                      (i32.const 4)
                                    )
                                  )
                                  (get_local $0)
                                )
                                (tee_local $1
                                  (i32.and
                                    (i32.shr_u
                                      (i32.add
                                        (tee_local $0
                                          (i32.shl
                                            (get_local $1)
                                            (get_local $4)
                                          )
                                        )
                                        (i32.const 245760)
                                      )
                                      (i32.const 16)
                                    )
                                    (i32.const 2)
                                  )
                                )
                              )
                            )
                            (i32.shr_u
                              (i32.shl
                                (get_local $0)
                                (get_local $1)
                              )
                              (i32.const 15)
                            )
                          )
                        )
                        (i32.const 7)
                      )
                    )
                    (i32.const 1)
                  )
                  (i32.shl
                    (get_local $0)
                    (i32.const 1)
                  )
                )
              )
              (i32.const 0)
            )
          )
          (i32.const 2)
        )
        (i32.const 5848)
      )
    )
    (i32.store offset=28
      (get_local $3)
      (get_local $1)
    )
    (i32.store offset=20
      (get_local $3)
      (i32.const 0)
    )
    (i32.store offset=16
      (get_local $3)
      (i32.const 0)
    )
    (block $do-once12
      (if
        (i32.and
          (tee_local $4
            (i32.load
              (i32.const 5548)
            )
          )
          (tee_local $5
            (i32.shl
              (i32.const 1)
              (get_local $1)
            )
          )
        )
        (block
          (set_local $0
            (i32.load
              (get_local $0)
            )
          )
          (set_local $4
            (i32.sub
              (i32.const 25)
              (i32.shr_u
                (get_local $1)
                (i32.const 1)
              )
            )
          )
          (set_local $1
            (i32.shl
              (get_local $2)
              (if i32
                (i32.eq
                  (get_local $1)
                  (i32.const 31)
                )
                (i32.const 0)
                (get_local $4)
              )
            )
          )
          (block $__rjto$1
            (block $__rjti$1
              (block $__rjti$0
                (loop $while-in15
                  (br_if $__rjti$1
                    (i32.eq
                      (i32.and
                        (i32.load offset=4
                          (get_local $0)
                        )
                        (i32.const -8)
                      )
                      (get_local $2)
                    )
                  )
                  (set_local $4
                    (i32.shl
                      (get_local $1)
                      (i32.const 1)
                    )
                  )
                  (br_if $__rjti$0
                    (i32.eqz
                      (tee_local $5
                        (i32.load
                          (tee_local $1
                            (i32.add
                              (i32.add
                                (get_local $0)
                                (i32.const 16)
                              )
                              (i32.shl
                                (i32.shr_u
                                  (get_local $1)
                                  (i32.const 31)
                                )
                                (i32.const 2)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                  (set_local $1
                    (get_local $4)
                  )
                  (set_local $0
                    (get_local $5)
                  )
                  (br $while-in15)
                )
              )
              (if
                (i32.lt_u
                  (get_local $1)
                  (i32.load
                    (i32.const 5560)
                  )
                )
                (call $_abort)
                (block
                  (i32.store
                    (get_local $1)
                    (get_local $3)
                  )
                  (i32.store offset=24
                    (get_local $3)
                    (get_local $0)
                  )
                  (i32.store offset=12
                    (get_local $3)
                    (get_local $3)
                  )
                  (i32.store offset=8
                    (get_local $3)
                    (get_local $3)
                  )
                  (br $do-once12)
                )
              )
              (br $__rjto$1)
            )
            (if
              (i32.and
                (i32.ge_u
                  (tee_local $2
                    (i32.load
                      (tee_local $1
                        (i32.add
                          (get_local $0)
                          (i32.const 8)
                        )
                      )
                    )
                  )
                  (tee_local $4
                    (i32.load
                      (i32.const 5560)
                    )
                  )
                )
                (i32.ge_u
                  (get_local $0)
                  (get_local $4)
                )
              )
              (block
                (i32.store offset=12
                  (get_local $2)
                  (get_local $3)
                )
                (i32.store
                  (get_local $1)
                  (get_local $3)
                )
                (i32.store offset=8
                  (get_local $3)
                  (get_local $2)
                )
                (i32.store offset=12
                  (get_local $3)
                  (get_local $0)
                )
                (i32.store offset=24
                  (get_local $3)
                  (i32.const 0)
                )
              )
              (call $_abort)
            )
          )
        )
        (block
          (i32.store
            (i32.const 5548)
            (i32.or
              (get_local $4)
              (get_local $5)
            )
          )
          (i32.store
            (get_local $0)
            (get_local $3)
          )
          (i32.store offset=24
            (get_local $3)
            (get_local $0)
          )
          (i32.store offset=12
            (get_local $3)
            (get_local $3)
          )
          (i32.store offset=8
            (get_local $3)
            (get_local $3)
          )
        )
      )
    )
    (i32.store
      (i32.const 5576)
      (tee_local $0
        (i32.add
          (i32.load
            (i32.const 5576)
          )
          (i32.const -1)
        )
      )
    )
    (if
      (get_local $0)
      (return)
      (set_local $0
        (i32.const 6000)
      )
    )
    (loop $while-in17
      (set_local $0
        (i32.add
          (tee_local $2
            (i32.load
              (get_local $0)
            )
          )
          (i32.const 8)
        )
      )
      (br_if $while-in17
        (get_local $2)
      )
    )
    (i32.store
      (i32.const 5576)
      (i32.const -1)
    )
  )
  (func $_cos (param $0 f64) (result f64)
    (local $1 i32)
    (local $2 i32)
    (local $3 i32)
    (set_local $2
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 16)
      )
    )
    (set_local $1
      (get_local $2)
    )
    (set_local $0
      (block $label$break$L1 f64
        (if f64
          (i32.lt_u
            (tee_local $3
              (i32.and
                (i32.wrap/i64
                  (i64.shr_u
                    (i64.reinterpret/f64
                      (get_local $0)
                    )
                    (i64.const 32)
                  )
                )
                (i32.const 2147483647)
              )
            )
            (i32.const 1072243196)
          )
          (if f64
            (i32.lt_u
              (get_local $3)
              (i32.const 1044816030)
            )
            (f64.const 1)
            (call $___cos
              (get_local $0)
              (f64.const 0)
            )
          )
          (block f64
            (drop
              (br_if $label$break$L1
                (f64.sub
                  (get_local $0)
                  (get_local $0)
                )
                (i32.gt_u
                  (get_local $3)
                  (i32.const 2146435071)
                )
              )
            )
            (block $switch-default
              (block $switch-case1
                (block $switch-case0
                  (block $switch-case
                    (br_table $switch-case $switch-case0 $switch-case1 $switch-default
                      (i32.sub
                        (i32.shr_s
                          (i32.shl
                            (i32.and
                              (i32.and
                                (call $___rem_pio2
                                  (get_local $0)
                                  (get_local $1)
                                )
                                (i32.const 255)
                              )
                              (i32.const 3)
                            )
                            (i32.const 24)
                          )
                          (i32.const 24)
                        )
                        (i32.const 0)
                      )
                    )
                  )
                  (br $label$break$L1
                    (call $___cos
                      (f64.load
                        (get_local $1)
                      )
                      (f64.load offset=8
                        (get_local $1)
                      )
                    )
                  )
                )
                (br $label$break$L1
                  (f64.neg
                    (call $___sin
                      (f64.load
                        (get_local $1)
                      )
                      (f64.load offset=8
                        (get_local $1)
                      )
                      (i32.const 1)
                    )
                  )
                )
              )
              (br $label$break$L1
                (f64.neg
                  (call $___cos
                    (f64.load
                      (get_local $1)
                    )
                    (f64.load offset=8
                      (get_local $1)
                    )
                  )
                )
              )
            )
            (call $___sin
              (f64.load
                (get_local $1)
              )
              (f64.load offset=8
                (get_local $1)
              )
              (i32.const 1)
            )
          )
        )
      )
    )
    (set_global $STACKTOP
      (get_local $2)
    )
    (get_local $0)
  )
  (func $_sin (param $0 f64) (result f64)
    (local $1 i32)
    (local $2 i32)
    (local $3 i32)
    (set_local $2
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 16)
      )
    )
    (set_local $1
      (get_local $2)
    )
    (block $label$break$L1
      (if
        (i32.lt_u
          (tee_local $3
            (i32.and
              (i32.wrap/i64
                (i64.shr_u
                  (i64.reinterpret/f64
                    (get_local $0)
                  )
                  (i64.const 32)
                )
              )
              (i32.const 2147483647)
            )
          )
          (i32.const 1072243196)
        )
        (if
          (i32.ge_u
            (get_local $3)
            (i32.const 1045430272)
          )
          (set_local $0
            (call $___sin
              (get_local $0)
              (f64.const 0)
              (i32.const 0)
            )
          )
        )
        (block
          (if
            (i32.gt_u
              (get_local $3)
              (i32.const 2146435071)
            )
            (block
              (set_local $0
                (f64.sub
                  (get_local $0)
                  (get_local $0)
                )
              )
              (br $label$break$L1)
            )
          )
          (block $switch-default
            (block $switch-case1
              (block $switch-case0
                (block $switch-case
                  (br_table $switch-case $switch-case0 $switch-case1 $switch-default
                    (i32.sub
                      (i32.shr_s
                        (i32.shl
                          (i32.and
                            (i32.and
                              (call $___rem_pio2
                                (get_local $0)
                                (get_local $1)
                              )
                              (i32.const 255)
                            )
                            (i32.const 3)
                          )
                          (i32.const 24)
                        )
                        (i32.const 24)
                      )
                      (i32.const 0)
                    )
                  )
                )
                (set_local $0
                  (call $___sin
                    (f64.load
                      (get_local $1)
                    )
                    (f64.load offset=8
                      (get_local $1)
                    )
                    (i32.const 1)
                  )
                )
                (br $label$break$L1)
              )
              (set_local $0
                (call $___cos
                  (f64.load
                    (get_local $1)
                  )
                  (f64.load offset=8
                    (get_local $1)
                  )
                )
              )
              (br $label$break$L1)
            )
            (set_local $0
              (f64.neg
                (call $___sin
                  (f64.load
                    (get_local $1)
                  )
                  (f64.load offset=8
                    (get_local $1)
                  )
                  (i32.const 1)
                )
              )
            )
            (br $label$break$L1)
          )
          (set_local $0
            (f64.neg
              (call $___cos
                (f64.load
                  (get_local $1)
                )
                (f64.load offset=8
                  (get_local $1)
                )
              )
            )
          )
        )
      )
    )
    (set_global $STACKTOP
      (get_local $2)
    )
    (get_local $0)
  )
  (func $__ZNKSt3__220__vector_base_commonILb1EE20__throw_length_errorEv (param $0 i32)
    (call $___assert_fail
      (i32.const 5130)
      (i32.const 5153)
      (i32.const 304)
      (i32.const 5236)
    )
  )
  (func $__Znwj (param $0 i32) (result i32)
    (local $1 i32)
    (if
      (i32.eqz
        (get_local $0)
      )
      (set_local $0
        (i32.const 1)
      )
    )
    (loop $while-in
      (block $while-out
        (if
          (tee_local $1
            (call $_malloc
              (get_local $0)
            )
          )
          (block
            (set_local $0
              (get_local $1)
            )
            (br $while-out)
          )
        )
        (if
          (tee_local $1
            (call $__ZSt15get_new_handlerv)
          )
          (block
            (call_indirect $FUNCSIG$v
              (i32.add
                (i32.and
                  (get_local $1)
                  (i32.const 0)
                )
                (i32.const 72)
              )
            )
            (br $while-in)
          )
          (set_local $0
            (i32.const 0)
          )
        )
      )
    )
    (get_local $0)
  )
  (func $__Znaj (param $0 i32) (result i32)
    (call $__Znwj
      (get_local $0)
    )
  )
  (func $__ZdlPv (param $0 i32)
    (call $_free
      (get_local $0)
    )
  )
  (func $__ZN10__cxxabiv117__class_type_infoD0Ev (param $0 i32)
    (call $__ZdlPv
      (get_local $0)
    )
  )
  (func $__ZNK10__cxxabiv117__class_type_info9can_catchEPKNS_16__shim_type_infoERPv (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
    (local $3 i32)
    (local $4 i32)
    (local $5 i32)
    (set_local $5
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 64)
      )
    )
    (set_local $3
      (get_local $5)
    )
    (set_local $0
      (if i32
        (i32.eq
          (get_local $0)
          (get_local $1)
        )
        (i32.const 1)
        (if i32
          (get_local $1)
          (if i32
            (tee_local $1
              (call $___dynamic_cast
                (get_local $1)
                (i32.const 1448)
                (i32.const 1432)
                (i32.const 0)
              )
            )
            (block i32
              (i64.store align=4
                (tee_local $4
                  (i32.add
                    (get_local $3)
                    (i32.const 4)
                  )
                )
                (i64.const 0)
              )
              (i64.store offset=8 align=4
                (get_local $4)
                (i64.const 0)
              )
              (i64.store offset=16 align=4
                (get_local $4)
                (i64.const 0)
              )
              (i64.store offset=24 align=4
                (get_local $4)
                (i64.const 0)
              )
              (i64.store offset=32 align=4
                (get_local $4)
                (i64.const 0)
              )
              (i64.store offset=40 align=4
                (get_local $4)
                (i64.const 0)
              )
              (i32.store offset=48
                (get_local $4)
                (i32.const 0)
              )
              (i32.store
                (get_local $3)
                (get_local $1)
              )
              (i32.store offset=8
                (get_local $3)
                (get_local $0)
              )
              (i32.store offset=12
                (get_local $3)
                (i32.const -1)
              )
              (i32.store offset=48
                (get_local $3)
                (i32.const 1)
              )
              (call_indirect $FUNCSIG$viiii
                (get_local $1)
                (get_local $3)
                (i32.load
                  (get_local $2)
                )
                (i32.const 1)
                (i32.add
                  (i32.and
                    (i32.load offset=28
                      (i32.load
                        (get_local $1)
                      )
                    )
                    (i32.const 3)
                  )
                  (i32.const 77)
                )
              )
              (if i32
                (i32.eq
                  (i32.load offset=24
                    (get_local $3)
                  )
                  (i32.const 1)
                )
                (block i32
                  (i32.store
                    (get_local $2)
                    (i32.load offset=16
                      (get_local $3)
                    )
                  )
                  (i32.const 1)
                )
                (i32.const 0)
              )
            )
            (i32.const 0)
          )
          (i32.const 0)
        )
      )
    )
    (set_global $STACKTOP
      (get_local $5)
    )
    (get_local $0)
  )
  (func $__ZNK10__cxxabiv117__class_type_info16search_above_dstEPNS_19__dynamic_cast_infoEPKvS4_ib (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32) (param $5 i32)
    (if
      (i32.eq
        (get_local $0)
        (i32.load offset=8
          (get_local $1)
        )
      )
      (call $__ZNK10__cxxabiv117__class_type_info29process_static_type_above_dstEPNS_19__dynamic_cast_infoEPKvS4_i
        (i32.const 0)
        (get_local $1)
        (get_local $2)
        (get_local $3)
        (get_local $4)
      )
    )
  )
  (func $__ZNK10__cxxabiv117__class_type_info16search_below_dstEPNS_19__dynamic_cast_infoEPKvib (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32)
    (block $do-once
      (if
        (i32.eq
          (get_local $0)
          (i32.load offset=8
            (get_local $1)
          )
        )
        (if
          (i32.eq
            (i32.load offset=4
              (get_local $1)
            )
            (get_local $2)
          )
          (if
            (i32.ne
              (i32.load
                (tee_local $0
                  (i32.add
                    (get_local $1)
                    (i32.const 28)
                  )
                )
              )
              (i32.const 1)
            )
            (i32.store
              (get_local $0)
              (get_local $3)
            )
          )
        )
        (if
          (i32.eq
            (get_local $0)
            (i32.load
              (get_local $1)
            )
          )
          (block
            (if
              (i32.ne
                (i32.load offset=16
                  (get_local $1)
                )
                (get_local $2)
              )
              (if
                (i32.ne
                  (i32.load
                    (tee_local $0
                      (i32.add
                        (get_local $1)
                        (i32.const 20)
                      )
                    )
                  )
                  (get_local $2)
                )
                (block
                  (i32.store offset=32
                    (get_local $1)
                    (get_local $3)
                  )
                  (i32.store
                    (get_local $0)
                    (get_local $2)
                  )
                  (i32.store
                    (tee_local $0
                      (i32.add
                        (get_local $1)
                        (i32.const 40)
                      )
                    )
                    (i32.add
                      (i32.load
                        (get_local $0)
                      )
                      (i32.const 1)
                    )
                  )
                  (if
                    (i32.eq
                      (i32.load offset=36
                        (get_local $1)
                      )
                      (i32.const 1)
                    )
                    (if
                      (i32.eq
                        (i32.load offset=24
                          (get_local $1)
                        )
                        (i32.const 2)
                      )
                      (i32.store8 offset=54
                        (get_local $1)
                        (i32.const 1)
                      )
                    )
                  )
                  (i32.store offset=44
                    (get_local $1)
                    (i32.const 4)
                  )
                  (br $do-once)
                )
              )
            )
            (if
              (i32.eq
                (get_local $3)
                (i32.const 1)
              )
              (i32.store offset=32
                (get_local $1)
                (i32.const 1)
              )
            )
          )
        )
      )
    )
  )
  (func $__ZNK10__cxxabiv117__class_type_info27has_unambiguous_public_baseEPNS_19__dynamic_cast_infoEPvi (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32)
    (if
      (i32.eq
        (get_local $0)
        (i32.load offset=8
          (get_local $1)
        )
      )
      (call $__ZNK10__cxxabiv117__class_type_info24process_found_base_classEPNS_19__dynamic_cast_infoEPvi
        (i32.const 0)
        (get_local $1)
        (get_local $2)
        (get_local $3)
      )
    )
  )
  (func $__ZNK10__cxxabiv117__class_type_info24process_found_base_classEPNS_19__dynamic_cast_infoEPvi (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32)
    (local $4 i32)
    (block $do-once
      (if
        (tee_local $4
          (i32.load
            (tee_local $0
              (i32.add
                (get_local $1)
                (i32.const 16)
              )
            )
          )
        )
        (block
          (if
            (i32.ne
              (get_local $4)
              (get_local $2)
            )
            (block
              (i32.store
                (tee_local $0
                  (i32.add
                    (get_local $1)
                    (i32.const 36)
                  )
                )
                (i32.add
                  (i32.load
                    (get_local $0)
                  )
                  (i32.const 1)
                )
              )
              (i32.store offset=24
                (get_local $1)
                (i32.const 2)
              )
              (i32.store8 offset=54
                (get_local $1)
                (i32.const 1)
              )
              (br $do-once)
            )
          )
          (if
            (i32.eq
              (i32.load
                (tee_local $0
                  (i32.add
                    (get_local $1)
                    (i32.const 24)
                  )
                )
              )
              (i32.const 2)
            )
            (i32.store
              (get_local $0)
              (get_local $3)
            )
          )
        )
        (block
          (i32.store
            (get_local $0)
            (get_local $2)
          )
          (i32.store offset=24
            (get_local $1)
            (get_local $3)
          )
          (i32.store offset=36
            (get_local $1)
            (i32.const 1)
          )
        )
      )
    )
  )
  (func $__ZNK10__cxxabiv117__class_type_info29process_static_type_above_dstEPNS_19__dynamic_cast_infoEPKvS4_i (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32)
    (i32.store8 offset=53
      (get_local $1)
      (i32.const 1)
    )
    (block $do-once
      (if
        (i32.eq
          (i32.load offset=4
            (get_local $1)
          )
          (get_local $3)
        )
        (block
          (i32.store8 offset=52
            (get_local $1)
            (i32.const 1)
          )
          (if
            (i32.eqz
              (tee_local $3
                (i32.load
                  (tee_local $0
                    (i32.add
                      (get_local $1)
                      (i32.const 16)
                    )
                  )
                )
              )
            )
            (block
              (i32.store
                (get_local $0)
                (get_local $2)
              )
              (i32.store offset=24
                (get_local $1)
                (get_local $4)
              )
              (i32.store offset=36
                (get_local $1)
                (i32.const 1)
              )
              (br_if $do-once
                (i32.eqz
                  (i32.and
                    (i32.eq
                      (i32.load offset=48
                        (get_local $1)
                      )
                      (i32.const 1)
                    )
                    (i32.eq
                      (get_local $4)
                      (i32.const 1)
                    )
                  )
                )
              )
              (i32.store8 offset=54
                (get_local $1)
                (i32.const 1)
              )
              (br $do-once)
            )
          )
          (if
            (i32.ne
              (get_local $3)
              (get_local $2)
            )
            (block
              (i32.store
                (tee_local $0
                  (i32.add
                    (get_local $1)
                    (i32.const 36)
                  )
                )
                (i32.add
                  (i32.load
                    (get_local $0)
                  )
                  (i32.const 1)
                )
              )
              (i32.store8 offset=54
                (get_local $1)
                (i32.const 1)
              )
              (br $do-once)
            )
          )
          (if
            (i32.eq
              (tee_local $0
                (i32.load
                  (tee_local $2
                    (i32.add
                      (get_local $1)
                      (i32.const 24)
                    )
                  )
                )
              )
              (i32.const 2)
            )
            (i32.store
              (get_local $2)
              (get_local $4)
            )
            (set_local $4
              (get_local $0)
            )
          )
          (if
            (i32.and
              (i32.eq
                (i32.load offset=48
                  (get_local $1)
                )
                (i32.const 1)
              )
              (i32.eq
                (get_local $4)
                (i32.const 1)
              )
            )
            (i32.store8 offset=54
              (get_local $1)
              (i32.const 1)
            )
          )
        )
      )
    )
  )
  (func $___dynamic_cast (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (result i32)
    (local $4 i32)
    (local $5 i32)
    (local $6 i32)
    (local $7 i32)
    (local $8 i32)
    (local $9 i32)
    (local $10 i32)
    (set_local $8
      (get_global $STACKTOP)
    )
    (set_global $STACKTOP
      (i32.add
        (get_global $STACKTOP)
        (i32.const 64)
      )
    )
    (set_local $6
      (i32.add
        (get_local $0)
        (i32.load
          (i32.add
            (tee_local $4
              (i32.load
                (get_local $0)
              )
            )
            (i32.const -8)
          )
        )
      )
    )
    (set_local $7
      (i32.load
        (i32.add
          (get_local $4)
          (i32.const -4)
        )
      )
    )
    (i32.store
      (tee_local $4
        (get_local $8)
      )
      (get_local $2)
    )
    (i32.store offset=4
      (get_local $4)
      (get_local $0)
    )
    (i32.store offset=8
      (get_local $4)
      (get_local $1)
    )
    (i32.store offset=12
      (get_local $4)
      (get_local $3)
    )
    (set_local $0
      (i32.add
        (get_local $4)
        (i32.const 20)
      )
    )
    (set_local $9
      (i32.add
        (get_local $4)
        (i32.const 24)
      )
    )
    (set_local $10
      (i32.add
        (get_local $4)
        (i32.const 28)
      )
    )
    (set_local $3
      (i32.add
        (get_local $4)
        (i32.const 32)
      )
    )
    (set_local $1
      (i32.add
        (get_local $4)
        (i32.const 40)
      )
    )
    (i64.store align=4
      (tee_local $5
        (i32.add
          (get_local $4)
          (i32.const 16)
        )
      )
      (i64.const 0)
    )
    (i64.store offset=8 align=4
      (get_local $5)
      (i64.const 0)
    )
    (i64.store offset=16 align=4
      (get_local $5)
      (i64.const 0)
    )
    (i64.store offset=24 align=4
      (get_local $5)
      (i64.const 0)
    )
    (i32.store offset=32
      (get_local $5)
      (i32.const 0)
    )
    (i32.store16 offset=36
      (get_local $5)
      (i32.const 0)
    )
    (i32.store8 offset=38
      (get_local $5)
      (i32.const 0)
    )
    (block $label$break$L1
      (set_local $0
        (if i32
          (i32.eq
            (get_local $7)
            (get_local $2)
          )
          (block i32
            (i32.store offset=48
              (get_local $4)
              (i32.const 1)
            )
            (call_indirect $FUNCSIG$viiiiii
              (get_local $2)
              (get_local $4)
              (get_local $6)
              (get_local $6)
              (i32.const 1)
              (i32.const 0)
              (i32.add
                (i32.and
                  (i32.load offset=20
                    (i32.load
                      (get_local $2)
                    )
                  )
                  (i32.const 3)
                )
                (i32.const 73)
              )
            )
            (if i32
              (i32.eq
                (i32.load
                  (get_local $9)
                )
                (i32.const 1)
              )
              (get_local $6)
              (i32.const 0)
            )
          )
          (block i32
            (call_indirect $FUNCSIG$viiiii
              (get_local $7)
              (get_local $4)
              (get_local $6)
              (i32.const 1)
              (i32.const 0)
              (i32.add
                (i32.and
                  (i32.load offset=24
                    (i32.load
                      (get_local $7)
                    )
                  )
                  (i32.const 3)
                )
                (i32.const 8)
              )
            )
            (block $switch
              (block $switch-default
                (block $switch-case0
                  (block $switch-case
                    (br_table $switch-case $switch-case0 $switch-default
                      (i32.sub
                        (i32.load offset=36
                          (get_local $4)
                        )
                        (i32.const 0)
                      )
                    )
                  )
                  (set_local $0
                    (i32.load
                      (get_local $0)
                    )
                  )
                  (if
                    (i32.eqz
                      (i32.and
                        (i32.and
                          (i32.eq
                            (i32.load
                              (get_local $1)
                            )
                            (i32.const 1)
                          )
                          (i32.eq
                            (i32.load
                              (get_local $10)
                            )
                            (i32.const 1)
                          )
                        )
                        (i32.eq
                          (i32.load
                            (get_local $3)
                          )
                          (i32.const 1)
                        )
                      )
                    )
                    (set_local $0
                      (i32.const 0)
                    )
                  )
                  (br $label$break$L1)
                )
                (br $switch)
              )
              (set_local $0
                (i32.const 0)
              )
              (br $label$break$L1)
            )
            (if
              (i32.ne
                (i32.load
                  (get_local $9)
                )
                (i32.const 1)
              )
              (if
                (i32.eqz
                  (i32.and
                    (i32.and
                      (i32.eqz
                        (i32.load
                          (get_local $1)
                        )
                      )
                      (i32.eq
                        (i32.load
                          (get_local $10)
                        )
                        (i32.const 1)
                      )
                    )
                    (i32.eq
                      (i32.load
                        (get_local $3)
                      )
                      (i32.const 1)
                    )
                  )
                )
                (block
                  (set_local $0
                    (i32.const 0)
                  )
                  (br $label$break$L1)
                )
              )
            )
            (i32.load
              (get_local $5)
            )
          )
        )
      )
    )
    (set_global $STACKTOP
      (get_local $8)
    )
    (get_local $0)
  )
  (func $__ZNK10__cxxabiv120__si_class_type_info16search_above_dstEPNS_19__dynamic_cast_infoEPKvS4_ib (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32) (param $5 i32)
    (local $6 i32)
    (if
      (i32.eq
        (get_local $0)
        (i32.load offset=8
          (get_local $1)
        )
      )
      (call $__ZNK10__cxxabiv117__class_type_info29process_static_type_above_dstEPNS_19__dynamic_cast_infoEPKvS4_i
        (i32.const 0)
        (get_local $1)
        (get_local $2)
        (get_local $3)
        (get_local $4)
      )
      (call_indirect $FUNCSIG$viiiiii
        (tee_local $6
          (i32.load offset=8
            (get_local $0)
          )
        )
        (get_local $1)
        (get_local $2)
        (get_local $3)
        (get_local $4)
        (get_local $5)
        (i32.add
          (i32.and
            (i32.load offset=20
              (i32.load
                (get_local $6)
              )
            )
            (i32.const 3)
          )
          (i32.const 73)
        )
      )
    )
  )
  (func $__ZNK10__cxxabiv120__si_class_type_info16search_below_dstEPNS_19__dynamic_cast_infoEPKvib (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32)
    (local $5 i32)
    (local $6 i32)
    (local $7 i32)
    (block $do-once
      (if
        (i32.eq
          (get_local $0)
          (i32.load offset=8
            (get_local $1)
          )
        )
        (if
          (i32.eq
            (i32.load offset=4
              (get_local $1)
            )
            (get_local $2)
          )
          (if
            (i32.ne
              (i32.load
                (tee_local $0
                  (i32.add
                    (get_local $1)
                    (i32.const 28)
                  )
                )
              )
              (i32.const 1)
            )
            (i32.store
              (get_local $0)
              (get_local $3)
            )
          )
        )
        (block
          (if
            (i32.ne
              (get_local $0)
              (i32.load
                (get_local $1)
              )
            )
            (block
              (call_indirect $FUNCSIG$viiiii
                (tee_local $0
                  (i32.load offset=8
                    (get_local $0)
                  )
                )
                (get_local $1)
                (get_local $2)
                (get_local $3)
                (get_local $4)
                (i32.add
                  (i32.and
                    (i32.load offset=24
                      (i32.load
                        (get_local $0)
                      )
                    )
                    (i32.const 3)
                  )
                  (i32.const 8)
                )
              )
              (br $do-once)
            )
          )
          (if
            (i32.ne
              (i32.load offset=16
                (get_local $1)
              )
              (get_local $2)
            )
            (if
              (i32.ne
                (i32.load
                  (tee_local $5
                    (i32.add
                      (get_local $1)
                      (i32.const 20)
                    )
                  )
                )
                (get_local $2)
              )
              (block
                (i32.store offset=32
                  (get_local $1)
                  (get_local $3)
                )
                (br_if $do-once
                  (i32.eq
                    (i32.load
                      (tee_local $3
                        (i32.add
                          (get_local $1)
                          (i32.const 44)
                        )
                      )
                    )
                    (i32.const 4)
                  )
                )
                (i32.store8
                  (tee_local $6
                    (i32.add
                      (get_local $1)
                      (i32.const 52)
                    )
                  )
                  (i32.const 0)
                )
                (i32.store8
                  (tee_local $7
                    (i32.add
                      (get_local $1)
                      (i32.const 53)
                    )
                  )
                  (i32.const 0)
                )
                (call_indirect $FUNCSIG$viiiiii
                  (tee_local $0
                    (i32.load offset=8
                      (get_local $0)
                    )
                  )
                  (get_local $1)
                  (get_local $2)
                  (get_local $2)
                  (i32.const 1)
                  (get_local $4)
                  (i32.add
                    (i32.and
                      (i32.load offset=20
                        (i32.load
                          (get_local $0)
                        )
                      )
                      (i32.const 3)
                    )
                    (i32.const 73)
                  )
                )
                (i32.store
                  (get_local $3)
                  (tee_local $0
                    (block $__rjto$1 i32
                      (block $__rjti$1
                        (set_local $0
                          (if i32
                            (i32.load8_s
                              (get_local $7)
                            )
                            (block i32
                              (br_if $__rjti$1
                                (i32.load8_s
                                  (get_local $6)
                                )
                              )
                              (i32.const 1)
                            )
                            (i32.const 0)
                          )
                        )
                        (i32.store
                          (get_local $5)
                          (get_local $2)
                        )
                        (i32.store
                          (tee_local $2
                            (i32.add
                              (get_local $1)
                              (i32.const 40)
                            )
                          )
                          (i32.add
                            (i32.load
                              (get_local $2)
                            )
                            (i32.const 1)
                          )
                        )
                        (if
                          (i32.eq
                            (i32.load offset=36
                              (get_local $1)
                            )
                            (i32.const 1)
                          )
                          (if
                            (i32.eq
                              (i32.load offset=24
                                (get_local $1)
                              )
                              (i32.const 2)
                            )
                            (block
                              (i32.store8 offset=54
                                (get_local $1)
                                (i32.const 1)
                              )
                              (br_if $__rjti$1
                                (get_local $0)
                              )
                              (br $__rjto$1
                                (i32.const 4)
                              )
                            )
                          )
                        )
                        (br_if $__rjti$1
                          (get_local $0)
                        )
                        (br $__rjto$1
                          (i32.const 4)
                        )
                      )
                      (i32.const 3)
                    )
                  )
                )
                (br $do-once)
              )
            )
          )
          (if
            (i32.eq
              (get_local $3)
              (i32.const 1)
            )
            (i32.store offset=32
              (get_local $1)
              (i32.const 1)
            )
          )
        )
      )
    )
  )
  (func $__ZNK10__cxxabiv120__si_class_type_info27has_unambiguous_public_baseEPNS_19__dynamic_cast_infoEPvi (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32)
    (local $4 i32)
    (if
      (i32.eq
        (get_local $0)
        (i32.load offset=8
          (get_local $1)
        )
      )
      (call $__ZNK10__cxxabiv117__class_type_info24process_found_base_classEPNS_19__dynamic_cast_infoEPvi
        (i32.const 0)
        (get_local $1)
        (get_local $2)
        (get_local $3)
      )
      (call_indirect $FUNCSIG$viiii
        (tee_local $4
          (i32.load offset=8
            (get_local $0)
          )
        )
        (get_local $1)
        (get_local $2)
        (get_local $3)
        (i32.add
          (i32.and
            (i32.load offset=28
              (i32.load
                (get_local $4)
              )
            )
            (i32.const 3)
          )
          (i32.const 77)
        )
      )
    )
  )
  (func $__ZNKSt9bad_alloc4whatEv (param $0 i32) (result i32)
    (i32.const 5387)
  )
  (func $__ZNSt9bad_allocC2Ev (param $0 i32)
    (i32.store
      (get_local $0)
      (i32.const 2328)
    )
  )
  (func $__ZSt15get_new_handlerv (result i32)
    (local $0 i32)
    (i32.store
      (i32.const 6040)
      (i32.add
        (tee_local $0
          (i32.load
            (i32.const 6040)
          )
        )
        (i32.const 0)
      )
    )
    (get_local $0)
  )
  (func $runPostSets
    (nop)
  )
  (func $_memset (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
    (local $3 i32)
    (local $4 i32)
    (local $5 i32)
    (local $6 i32)
    (set_local $4
      (i32.add
        (get_local $0)
        (get_local $2)
      )
    )
    (set_local $1
      (i32.and
        (get_local $1)
        (i32.const 255)
      )
    )
    (if
      (i32.ge_s
        (get_local $2)
        (i32.const 67)
      )
      (block
        (loop $while-in
          (if
            (i32.and
              (get_local $0)
              (i32.const 3)
            )
            (block
              (i32.store8
                (get_local $0)
                (get_local $1)
              )
              (set_local $0
                (i32.add
                  (get_local $0)
                  (i32.const 1)
                )
              )
              (br $while-in)
            )
          )
        )
        (set_local $6
          (i32.sub
            (tee_local $5
              (i32.and
                (get_local $4)
                (i32.const -4)
              )
            )
            (i32.const 64)
          )
        )
        (set_local $3
          (i32.or
            (i32.or
              (i32.or
                (get_local $1)
                (i32.shl
                  (get_local $1)
                  (i32.const 8)
                )
              )
              (i32.shl
                (get_local $1)
                (i32.const 16)
              )
            )
            (i32.shl
              (get_local $1)
              (i32.const 24)
            )
          )
        )
        (loop $while-in1
          (if
            (i32.le_s
              (get_local $0)
              (get_local $6)
            )
            (block
              (i32.store
                (get_local $0)
                (get_local $3)
              )
              (i32.store offset=4
                (get_local $0)
                (get_local $3)
              )
              (i32.store offset=8
                (get_local $0)
                (get_local $3)
              )
              (i32.store offset=12
                (get_local $0)
                (get_local $3)
              )
              (i32.store offset=16
                (get_local $0)
                (get_local $3)
              )
              (i32.store offset=20
                (get_local $0)
                (get_local $3)
              )
              (i32.store offset=24
                (get_local $0)
                (get_local $3)
              )
              (i32.store offset=28
                (get_local $0)
                (get_local $3)
              )
              (i32.store offset=32
                (get_local $0)
                (get_local $3)
              )
              (i32.store offset=36
                (get_local $0)
                (get_local $3)
              )
              (i32.store offset=40
                (get_local $0)
                (get_local $3)
              )
              (i32.store offset=44
                (get_local $0)
                (get_local $3)
              )
              (i32.store offset=48
                (get_local $0)
                (get_local $3)
              )
              (i32.store offset=52
                (get_local $0)
                (get_local $3)
              )
              (i32.store offset=56
                (get_local $0)
                (get_local $3)
              )
              (i32.store offset=60
                (get_local $0)
                (get_local $3)
              )
              (set_local $0
                (i32.add
                  (get_local $0)
                  (i32.const 64)
                )
              )
              (br $while-in1)
            )
          )
        )
        (loop $while-in3
          (if
            (i32.lt_s
              (get_local $0)
              (get_local $5)
            )
            (block
              (i32.store
                (get_local $0)
                (get_local $3)
              )
              (set_local $0
                (i32.add
                  (get_local $0)
                  (i32.const 4)
                )
              )
              (br $while-in3)
            )
          )
        )
      )
    )
    (loop $while-in5
      (if
        (i32.lt_s
          (get_local $0)
          (get_local $4)
        )
        (block
          (i32.store8
            (get_local $0)
            (get_local $1)
          )
          (set_local $0
            (i32.add
              (get_local $0)
              (i32.const 1)
            )
          )
          (br $while-in5)
        )
      )
    )
    (i32.sub
      (get_local $4)
      (get_local $2)
    )
  )
  (func $_pthread_self (result i32)
    (i32.const 0)
  )
  (func $_sbrk (param $0 i32) (result i32)
    (local $1 i32)
    (local $2 i32)
    (set_local $1
      (i32.add
        (tee_local $2
          (i32.load
            (get_global $DYNAMICTOP_PTR)
          )
        )
        (tee_local $0
          (i32.and
            (i32.add
              (get_local $0)
              (i32.const 15)
            )
            (i32.const -16)
          )
        )
      )
    )
    (if
      (i32.or
        (i32.and
          (i32.gt_s
            (get_local $0)
            (i32.const 0)
          )
          (i32.lt_s
            (get_local $1)
            (get_local $2)
          )
        )
        (i32.lt_s
          (get_local $1)
          (i32.const 0)
        )
      )
      (block
        (drop
          (call $abortOnCannotGrowMemory)
        )
        (call $___setErrNo
          (i32.const 12)
        )
        (return
          (i32.const -1)
        )
      )
    )
    (i32.store
      (get_global $DYNAMICTOP_PTR)
      (get_local $1)
    )
    (if
      (i32.gt_s
        (get_local $1)
        (call $getTotalMemory)
      )
      (if
        (i32.eqz
          (call $enlargeMemory)
        )
        (block
          (call $___setErrNo
            (i32.const 12)
          )
          (i32.store
            (get_global $DYNAMICTOP_PTR)
            (get_local $2)
          )
          (return
            (i32.const -1)
          )
        )
      )
    )
    (get_local $2)
  )
  (func $_memcpy (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
    (local $3 i32)
    (local $4 i32)
    (local $5 i32)
    (if
      (i32.ge_s
        (get_local $2)
        (i32.const 8192)
      )
      (return
        (call $_emscripten_memcpy_big
          (get_local $0)
          (get_local $1)
          (get_local $2)
        )
      )
    )
    (set_local $4
      (get_local $0)
    )
    (set_local $3
      (i32.add
        (get_local $0)
        (get_local $2)
      )
    )
    (if
      (i32.eq
        (i32.and
          (get_local $0)
          (i32.const 3)
        )
        (i32.and
          (get_local $1)
          (i32.const 3)
        )
      )
      (block
        (loop $while-in
          (if
            (i32.and
              (get_local $0)
              (i32.const 3)
            )
            (block
              (if
                (i32.eqz
                  (get_local $2)
                )
                (return
                  (get_local $4)
                )
              )
              (i32.store8
                (get_local $0)
                (i32.load8_s
                  (get_local $1)
                )
              )
              (set_local $0
                (i32.add
                  (get_local $0)
                  (i32.const 1)
                )
              )
              (set_local $1
                (i32.add
                  (get_local $1)
                  (i32.const 1)
                )
              )
              (set_local $2
                (i32.sub
                  (get_local $2)
                  (i32.const 1)
                )
              )
              (br $while-in)
            )
          )
        )
        (set_local $5
          (i32.sub
            (tee_local $2
              (i32.and
                (get_local $3)
                (i32.const -4)
              )
            )
            (i32.const 64)
          )
        )
        (loop $while-in1
          (if
            (i32.le_s
              (get_local $0)
              (get_local $5)
            )
            (block
              (i32.store
                (get_local $0)
                (i32.load
                  (get_local $1)
                )
              )
              (i32.store offset=4
                (get_local $0)
                (i32.load offset=4
                  (get_local $1)
                )
              )
              (i32.store offset=8
                (get_local $0)
                (i32.load offset=8
                  (get_local $1)
                )
              )
              (i32.store offset=12
                (get_local $0)
                (i32.load offset=12
                  (get_local $1)
                )
              )
              (i32.store offset=16
                (get_local $0)
                (i32.load offset=16
                  (get_local $1)
                )
              )
              (i32.store offset=20
                (get_local $0)
                (i32.load offset=20
                  (get_local $1)
                )
              )
              (i32.store offset=24
                (get_local $0)
                (i32.load offset=24
                  (get_local $1)
                )
              )
              (i32.store offset=28
                (get_local $0)
                (i32.load offset=28
                  (get_local $1)
                )
              )
              (i32.store offset=32
                (get_local $0)
                (i32.load offset=32
                  (get_local $1)
                )
              )
              (i32.store offset=36
                (get_local $0)
                (i32.load offset=36
                  (get_local $1)
                )
              )
              (i32.store offset=40
                (get_local $0)
                (i32.load offset=40
                  (get_local $1)
                )
              )
              (i32.store offset=44
                (get_local $0)
                (i32.load offset=44
                  (get_local $1)
                )
              )
              (i32.store offset=48
                (get_local $0)
                (i32.load offset=48
                  (get_local $1)
                )
              )
              (i32.store offset=52
                (get_local $0)
                (i32.load offset=52
                  (get_local $1)
                )
              )
              (i32.store offset=56
                (get_local $0)
                (i32.load offset=56
                  (get_local $1)
                )
              )
              (i32.store offset=60
                (get_local $0)
                (i32.load offset=60
                  (get_local $1)
                )
              )
              (set_local $0
                (i32.add
                  (get_local $0)
                  (i32.const 64)
                )
              )
              (set_local $1
                (i32.add
                  (get_local $1)
                  (i32.const 64)
                )
              )
              (br $while-in1)
            )
          )
        )
        (loop $while-in3
          (if
            (i32.lt_s
              (get_local $0)
              (get_local $2)
            )
            (block
              (i32.store
                (get_local $0)
                (i32.load
                  (get_local $1)
                )
              )
              (set_local $0
                (i32.add
                  (get_local $0)
                  (i32.const 4)
                )
              )
              (set_local $1
                (i32.add
                  (get_local $1)
                  (i32.const 4)
                )
              )
              (br $while-in3)
            )
          )
        )
      )
      (block
        (set_local $2
          (i32.sub
            (get_local $3)
            (i32.const 4)
          )
        )
        (loop $while-in5
          (if
            (i32.lt_s
              (get_local $0)
              (get_local $2)
            )
            (block
              (i32.store8
                (get_local $0)
                (i32.load8_s
                  (get_local $1)
                )
              )
              (i32.store8 offset=1
                (get_local $0)
                (i32.load8_s offset=1
                  (get_local $1)
                )
              )
              (i32.store8 offset=2
                (get_local $0)
                (i32.load8_s offset=2
                  (get_local $1)
                )
              )
              (i32.store8 offset=3
                (get_local $0)
                (i32.load8_s offset=3
                  (get_local $1)
                )
              )
              (set_local $0
                (i32.add
                  (get_local $0)
                  (i32.const 4)
                )
              )
              (set_local $1
                (i32.add
                  (get_local $1)
                  (i32.const 4)
                )
              )
              (br $while-in5)
            )
          )
        )
      )
    )
    (loop $while-in7
      (if
        (i32.lt_s
          (get_local $0)
          (get_local $3)
        )
        (block
          (i32.store8
            (get_local $0)
            (i32.load8_s
              (get_local $1)
            )
          )
          (set_local $0
            (i32.add
              (get_local $0)
              (i32.const 1)
            )
          )
          (set_local $1
            (i32.add
              (get_local $1)
              (i32.const 1)
            )
          )
          (br $while-in7)
        )
      )
    )
    (get_local $4)
  )
  (func $_memmove (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
    (local $3 i32)
    (if
      (i32.and
        (i32.lt_s
          (get_local $1)
          (get_local $0)
        )
        (i32.lt_s
          (get_local $0)
          (i32.add
            (get_local $1)
            (get_local $2)
          )
        )
      )
      (block
        (set_local $3
          (get_local $0)
        )
        (set_local $1
          (i32.add
            (get_local $1)
            (get_local $2)
          )
        )
        (set_local $0
          (i32.add
            (get_local $0)
            (get_local $2)
          )
        )
        (loop $while-in
          (if
            (i32.gt_s
              (get_local $2)
              (i32.const 0)
            )
            (block
              (set_local $2
                (i32.sub
                  (get_local $2)
                  (i32.const 1)
                )
              )
              (i32.store8
                (tee_local $0
                  (i32.sub
                    (get_local $0)
                    (i32.const 1)
                  )
                )
                (i32.load8_s
                  (tee_local $1
                    (i32.sub
                      (get_local $1)
                      (i32.const 1)
                    )
                  )
                )
              )
              (br $while-in)
            )
          )
        )
        (set_local $0
          (get_local $3)
        )
      )
      (drop
        (call $_memcpy
          (get_local $0)
          (get_local $1)
          (get_local $2)
        )
      )
    )
    (get_local $0)
  )
  (func $dynCall_iiii (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (result i32)
    (call_indirect $FUNCSIG$iiii
      (get_local $1)
      (get_local $2)
      (get_local $3)
      (i32.add
        (i32.and
          (get_local $0)
          (i32.const 7)
        )
        (i32.const 0)
      )
    )
  )
  (func $dynCall_viiiii (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32) (param $5 i32)
    (call_indirect $FUNCSIG$viiiii
      (get_local $1)
      (get_local $2)
      (get_local $3)
      (get_local $4)
      (get_local $5)
      (i32.add
        (i32.and
          (get_local $0)
          (i32.const 3)
        )
        (i32.const 8)
      )
    )
  )
  (func $dynCall_vi (param $0 i32) (param $1 i32)
    (call_indirect $FUNCSIG$vi
      (get_local $1)
      (i32.add
        (i32.and
          (get_local $0)
          (i32.const 15)
        )
        (i32.const 12)
      )
    )
  )
  (func $dynCall_iiiiddi (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 f64) (param $5 f64) (param $6 i32) (result i32)
    (call_indirect $FUNCSIG$iiiiddi
      (get_local $1)
      (get_local $2)
      (get_local $3)
      (get_local $4)
      (get_local $5)
      (get_local $6)
      (i32.add
        (i32.and
          (get_local $0)
          (i32.const 7)
        )
        (i32.const 28)
      )
    )
  )
  (func $dynCall_vii (param $0 i32) (param $1 i32) (param $2 i32)
    (call_indirect $FUNCSIG$vii
      (get_local $1)
      (get_local $2)
      (i32.add
        (i32.and
          (get_local $0)
          (i32.const 15)
        )
        (i32.const 36)
      )
    )
  )
  (func $dynCall_ii (param $0 i32) (param $1 i32) (result i32)
    (call_indirect $FUNCSIG$ii
      (get_local $1)
      (i32.add
        (i32.and
          (get_local $0)
          (i32.const 3)
        )
        (i32.const 52)
      )
    )
  )
  (func $dynCall_viii (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32)
    (call_indirect $FUNCSIG$viii
      (get_local $1)
      (get_local $2)
      (get_local $3)
      (i32.add
        (i32.and
          (get_local $0)
          (i32.const 15)
        )
        (i32.const 56)
      )
    )
  )
  (func $dynCall_v (param $0 i32)
    (call_indirect $FUNCSIG$v
      (i32.add
        (i32.and
          (get_local $0)
          (i32.const 0)
        )
        (i32.const 72)
      )
    )
  )
  (func $dynCall_viiiiii (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32) (param $5 i32) (param $6 i32)
    (call_indirect $FUNCSIG$viiiiii
      (get_local $1)
      (get_local $2)
      (get_local $3)
      (get_local $4)
      (get_local $5)
      (get_local $6)
      (i32.add
        (i32.and
          (get_local $0)
          (i32.const 3)
        )
        (i32.const 73)
      )
    )
  )
  (func $dynCall_viiii (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32)
    (call_indirect $FUNCSIG$viiii
      (get_local $1)
      (get_local $2)
      (get_local $3)
      (get_local $4)
      (i32.add
        (i32.and
          (get_local $0)
          (i32.const 3)
        )
        (i32.const 77)
      )
    )
  )
  (func $b0 (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
    (call $abort
      (i32.const 0)
    )
    (i32.const 0)
  )
  (func $b1 (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32)
    (call $abort
      (i32.const 1)
    )
  )
  (func $b2 (param $0 i32)
    (call $abort
      (i32.const 2)
    )
  )
  (func $b3 (param $0 i32) (param $1 i32) (param $2 i32) (param $3 f64) (param $4 f64) (param $5 i32) (result i32)
    (call $abort
      (i32.const 3)
    )
    (i32.const 0)
  )
  (func $b4 (param $0 i32) (param $1 i32)
    (call $abort
      (i32.const 4)
    )
  )
  (func $b5 (param $0 i32) (result i32)
    (call $abort
      (i32.const 5)
    )
    (i32.const 0)
  )
  (func $b6 (param $0 i32) (param $1 i32) (param $2 i32)
    (call $abort
      (i32.const 6)
    )
  )
  (func $b7
    (call $abort
      (i32.const 7)
    )
  )
  (func $b8 (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32) (param $5 i32)
    (call $abort
      (i32.const 8)
    )
  )
  (func $b9 (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32)
    (call $abort
      (i32.const 9)
    )
  )
)
