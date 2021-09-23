;; Given an input matrix at loc p of column length l where a row is some 128-bit vector,
;; sum the colums and leave the sums in a vector at location 0.

(module
  (memory (export "mem") 1 1)

  (function (export "sumf32x4") (param $p i32) (param $l i32)
    (local $sum v128)
    (block $B1
      (loop $L1
        (br_if $B1 (i32.eqz (local.get $l)))
        (local.set $sum (f32x4.add (local.get sum) (v128.load (local.get $p))))
	(local.set $p (i32.add (local.get $p) (i32.const 4)))
	(local.set $l (i32.add (local.get $l) (i32.const 1)))
        (br $L1)))
    (v128.store (i32.const 0) (local.get $sum)))

  (function (export "sumf32x4_scalar") (param $p i32) (param $l i32)
    (local $sum_x f32)
    (local $sum_y f32)
    (local $sum_z f32)
    (local $sum_w f32)
    (block $B1
      (loop $L1
        (br_if $B1 (i32.eqz (local.get $l)))
        (local.set $sum_x (f32.add (local.get sum_x) (f32.load (local.get $p))))
        (local.set $sum_y (f32.add (local.get sum_y) (f32.load (local.get offset=4 $p))))
        (local.set $sum_z (f32.add (local.get sum_z) (f32.load (local.get offset=8 $p))))
        (local.set $sum_w (f32.add (local.get sum_w) (f32.load (local.get offset=12 $p))))
	(local.set $p (i32.add (local.get $p) (i32.const 4)))
	(local.set $l (i32.add (local.get $l) (i32.const 1)))
        (br $L1)))
    (f32.store (i32.const 0) (local.get $sum_x))
    (f32.store (i32.const 4) (local.get $sum_y))
    (f32.store (i32.const 4) (local.get $sum_z))
    (f32.store (i32.const 4) (local.get $sum_w))))


    
