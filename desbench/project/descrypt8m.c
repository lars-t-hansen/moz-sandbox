typedef unsigned long long WORD;
static const WORD s_box_0[64]={
0x808200ULL,
0x0ULL,
0x8000ULL,
0x808202ULL,
0x808002ULL,
0x8202ULL,
0x2ULL,
0x8000ULL,
0x200ULL,
0x808200ULL,
0x808202ULL,
0x200ULL,
0x800202ULL,
0x808002ULL,
0x800000ULL,
0x2ULL,
0x202ULL,
0x800200ULL,
0x800200ULL,
0x8200ULL,
0x8200ULL,
0x808000ULL,
0x808000ULL,
0x800202ULL,
0x8002ULL,
0x800002ULL,
0x800002ULL,
0x8002ULL,
0x0ULL,
0x202ULL,
0x8202ULL,
0x800000ULL,
0x8000ULL,
0x808202ULL,
0x2ULL,
0x808000ULL,
0x808200ULL,
0x800000ULL,
0x800000ULL,
0x200ULL,
0x808002ULL,
0x8000ULL,
0x8200ULL,
0x800002ULL,
0x200ULL,
0x2ULL,
0x800202ULL,
0x8202ULL,
0x808202ULL,
0x8002ULL,
0x808000ULL,
0x800202ULL,
0x800002ULL,
0x202ULL,
0x8202ULL,
0x808200ULL,
0x202ULL,
0x800200ULL,
0x800200ULL,
0x0ULL,
0x8002ULL,
0x8200ULL,
0x0ULL,
0x808002ULL,
};
static const WORD s_box_1[64]={
0x40084010ULL,
0x40004000ULL,
0x4000ULL,
0x84010ULL,
0x80000ULL,
0x10ULL,
0x40080010ULL,
0x40004010ULL,
0x40000010ULL,
0x40084010ULL,
0x40084000ULL,
0x40000000ULL,
0x40004000ULL,
0x80000ULL,
0x10ULL,
0x40080010ULL,
0x84000ULL,
0x80010ULL,
0x40004010ULL,
0x0ULL,
0x40000000ULL,
0x4000ULL,
0x84010ULL,
0x40080000ULL,
0x80010ULL,
0x40000010ULL,
0x0ULL,
0x84000ULL,
0x4010ULL,
0x40084000ULL,
0x40080000ULL,
0x4010ULL,
0x0ULL,
0x84010ULL,
0x40080010ULL,
0x80000ULL,
0x40004010ULL,
0x40080000ULL,
0x40084000ULL,
0x4000ULL,
0x40080000ULL,
0x40004000ULL,
0x10ULL,
0x40084010ULL,
0x84010ULL,
0x10ULL,
0x4000ULL,
0x40000000ULL,
0x4010ULL,
0x40084000ULL,
0x80000ULL,
0x40000010ULL,
0x80010ULL,
0x40004010ULL,
0x40000010ULL,
0x80010ULL,
0x84000ULL,
0x0ULL,
0x40004000ULL,
0x4010ULL,
0x40000000ULL,
0x40080010ULL,
0x40084010ULL,
0x84000ULL,
};
static const WORD s_box_2[64]={
0x104ULL,
0x4010100ULL,
0x0ULL,
0x4010004ULL,
0x4000100ULL,
0x0ULL,
0x10104ULL,
0x4000100ULL,
0x10004ULL,
0x4000004ULL,
0x4000004ULL,
0x10000ULL,
0x4010104ULL,
0x10004ULL,
0x4010000ULL,
0x104ULL,
0x4000000ULL,
0x4ULL,
0x4010100ULL,
0x100ULL,
0x10100ULL,
0x4010000ULL,
0x4010004ULL,
0x10104ULL,
0x4000104ULL,
0x10100ULL,
0x10000ULL,
0x4000104ULL,
0x4ULL,
0x4010104ULL,
0x100ULL,
0x4000000ULL,
0x4010100ULL,
0x4000000ULL,
0x10004ULL,
0x104ULL,
0x10000ULL,
0x4010100ULL,
0x4000100ULL,
0x0ULL,
0x100ULL,
0x10004ULL,
0x4010104ULL,
0x4000100ULL,
0x4000004ULL,
0x100ULL,
0x0ULL,
0x4010004ULL,
0x4000104ULL,
0x10000ULL,
0x4000000ULL,
0x4010104ULL,
0x4ULL,
0x10104ULL,
0x10100ULL,
0x4000004ULL,
0x4010000ULL,
0x4000104ULL,
0x104ULL,
0x4010000ULL,
0x10104ULL,
0x4ULL,
0x4010004ULL,
0x10100ULL,
};
static const WORD s_box_3[64]={
0x80401000ULL,
0x80001040ULL,
0x80001040ULL,
0x40ULL,
0x401040ULL,
0x80400040ULL,
0x80400000ULL,
0x80001000ULL,
0x0ULL,
0x401000ULL,
0x401000ULL,
0x80401040ULL,
0x80000040ULL,
0x0ULL,
0x400040ULL,
0x80400000ULL,
0x80000000ULL,
0x1000ULL,
0x400000ULL,
0x80401000ULL,
0x40ULL,
0x400000ULL,
0x80001000ULL,
0x1040ULL,
0x80400040ULL,
0x80000000ULL,
0x1040ULL,
0x400040ULL,
0x1000ULL,
0x401040ULL,
0x80401040ULL,
0x80000040ULL,
0x400040ULL,
0x80400000ULL,
0x401000ULL,
0x80401040ULL,
0x80000040ULL,
0x0ULL,
0x0ULL,
0x401000ULL,
0x1040ULL,
0x400040ULL,
0x80400040ULL,
0x80000000ULL,
0x80401000ULL,
0x80001040ULL,
0x80001040ULL,
0x40ULL,
0x80401040ULL,
0x80000040ULL,
0x80000000ULL,
0x1000ULL,
0x80400000ULL,
0x80001000ULL,
0x401040ULL,
0x80400040ULL,
0x80001000ULL,
0x1040ULL,
0x400000ULL,
0x80401000ULL,
0x40ULL,
0x400000ULL,
0x1000ULL,
0x401040ULL,
};
static const WORD s_box_4[64]={
0x80ULL,
0x1040080ULL,
0x1040000ULL,
0x21000080ULL,
0x40000ULL,
0x80ULL,
0x20000000ULL,
0x1040000ULL,
0x20040080ULL,
0x40000ULL,
0x1000080ULL,
0x20040080ULL,
0x21000080ULL,
0x21040000ULL,
0x40080ULL,
0x20000000ULL,
0x1000000ULL,
0x20040000ULL,
0x20040000ULL,
0x0ULL,
0x20000080ULL,
0x21040080ULL,
0x21040080ULL,
0x1000080ULL,
0x21040000ULL,
0x20000080ULL,
0x0ULL,
0x21000000ULL,
0x1040080ULL,
0x1000000ULL,
0x21000000ULL,
0x40080ULL,
0x40000ULL,
0x21000080ULL,
0x80ULL,
0x1000000ULL,
0x20000000ULL,
0x1040000ULL,
0x21000080ULL,
0x20040080ULL,
0x1000080ULL,
0x20000000ULL,
0x21040000ULL,
0x1040080ULL,
0x20040080ULL,
0x80ULL,
0x1000000ULL,
0x21040000ULL,
0x21040080ULL,
0x40080ULL,
0x21000000ULL,
0x21040080ULL,
0x1040000ULL,
0x0ULL,
0x20040000ULL,
0x21000000ULL,
0x40080ULL,
0x1000080ULL,
0x20000080ULL,
0x40000ULL,
0x0ULL,
0x20040000ULL,
0x1040080ULL,
0x20000080ULL,
};
static const WORD s_box_5[64]={
0x10000008ULL,
0x10200000ULL,
0x2000ULL,
0x10202008ULL,
0x10200000ULL,
0x8ULL,
0x10202008ULL,
0x200000ULL,
0x10002000ULL,
0x202008ULL,
0x200000ULL,
0x10000008ULL,
0x200008ULL,
0x10002000ULL,
0x10000000ULL,
0x2008ULL,
0x0ULL,
0x200008ULL,
0x10002008ULL,
0x2000ULL,
0x202000ULL,
0x10002008ULL,
0x8ULL,
0x10200008ULL,
0x10200008ULL,
0x0ULL,
0x202008ULL,
0x10202000ULL,
0x2008ULL,
0x202000ULL,
0x10202000ULL,
0x10000000ULL,
0x10002000ULL,
0x8ULL,
0x10200008ULL,
0x202000ULL,
0x10202008ULL,
0x200000ULL,
0x2008ULL,
0x10000008ULL,
0x200000ULL,
0x10002000ULL,
0x10000000ULL,
0x2008ULL,
0x10000008ULL,
0x10202008ULL,
0x202000ULL,
0x10200000ULL,
0x202008ULL,
0x10202000ULL,
0x0ULL,
0x10200008ULL,
0x8ULL,
0x2000ULL,
0x10200000ULL,
0x202008ULL,
0x2000ULL,
0x200008ULL,
0x10002008ULL,
0x0ULL,
0x10202000ULL,
0x10000000ULL,
0x200008ULL,
0x10002008ULL,
};
static const WORD s_box_6[64]={
0x100000ULL,
0x2100001ULL,
0x2000401ULL,
0x0ULL,
0x400ULL,
0x2000401ULL,
0x100401ULL,
0x2100400ULL,
0x2100401ULL,
0x100000ULL,
0x0ULL,
0x2000001ULL,
0x1ULL,
0x2000000ULL,
0x2100001ULL,
0x401ULL,
0x2000400ULL,
0x100401ULL,
0x100001ULL,
0x2000400ULL,
0x2000001ULL,
0x2100000ULL,
0x2100400ULL,
0x100001ULL,
0x2100000ULL,
0x400ULL,
0x401ULL,
0x2100401ULL,
0x100400ULL,
0x1ULL,
0x2000000ULL,
0x100400ULL,
0x2000000ULL,
0x100400ULL,
0x100000ULL,
0x2000401ULL,
0x2000401ULL,
0x2100001ULL,
0x2100001ULL,
0x1ULL,
0x100001ULL,
0x2000000ULL,
0x2000400ULL,
0x100000ULL,
0x2100400ULL,
0x401ULL,
0x100401ULL,
0x2100400ULL,
0x401ULL,
0x2000001ULL,
0x2100401ULL,
0x2100000ULL,
0x100400ULL,
0x0ULL,
0x1ULL,
0x2100401ULL,
0x0ULL,
0x100401ULL,
0x2100000ULL,
0x400ULL,
0x2000001ULL,
0x2000400ULL,
0x400ULL,
0x100001ULL,
};
static const WORD s_box_7[64]={
0x8000820ULL,
0x800ULL,
0x20000ULL,
0x8020820ULL,
0x8000000ULL,
0x8000820ULL,
0x20ULL,
0x8000000ULL,
0x20020ULL,
0x8020000ULL,
0x8020820ULL,
0x20800ULL,
0x8020800ULL,
0x20820ULL,
0x800ULL,
0x20ULL,
0x8020000ULL,
0x8000020ULL,
0x8000800ULL,
0x820ULL,
0x20800ULL,
0x20020ULL,
0x8020020ULL,
0x8020800ULL,
0x820ULL,
0x0ULL,
0x0ULL,
0x8020020ULL,
0x8000020ULL,
0x8000800ULL,
0x20820ULL,
0x20000ULL,
0x20820ULL,
0x20000ULL,
0x8020800ULL,
0x800ULL,
0x20ULL,
0x8020020ULL,
0x800ULL,
0x20820ULL,
0x8000800ULL,
0x20ULL,
0x8000020ULL,
0x8020000ULL,
0x8020020ULL,
0x8000000ULL,
0x20000ULL,
0x8000820ULL,
0x0ULL,
0x8020820ULL,
0x20020ULL,
0x8000020ULL,
0x8020000ULL,
0x8000800ULL,
0x8000820ULL,
0x0ULL,
0x8020820ULL,
0x20800ULL,
0x20800ULL,
0x820ULL,
0x820ULL,
0x20020ULL,
0x8000000ULL,
0x8020800ULL,
};
static const WORD key_0 = 0x1b02effc7072ULL;
static const WORD key_1 = 0x79aed9dbc9e5ULL;
static const WORD key_2 = 0x55fc8a42cf99ULL;
static const WORD key_3 = 0x72add6db351dULL;
static const WORD key_4 = 0x7cec07eb53a8ULL;
static const WORD key_5 = 0x63a53e507b2fULL;
static const WORD key_6 = 0xec84b7f618bcULL;
static const WORD key_7 = 0xf78a3ac13bfbULL;
static const WORD key_8 = 0xe0dbebede781ULL;
static const WORD key_9 = 0xb1f347ba464fULL;
static const WORD key_10 = 0x215fd3ded386ULL;
static const WORD key_11 = 0x7571f59467e9ULL;
static const WORD key_12 = 0x97c5d1faba41ULL;
static const WORD key_13 = 0x5f43b7f2e73aULL;
static const WORD key_14 = 0xbf918d3d3f0aULL;
static const WORD key_15 = 0xcb3d8b0e17f5ULL;
WORD des_encrypt( WORD text ) {
WORD x_1, x_2, x_3, x_4, x_5, x_6, x_7, x_8, x_9, x_a, x_b, x_c, x_d, x_e, x_f, 
x_g, x_h, x_i, x_j, x_k, x_l, x_m, x_n, x_o, x_p, x_q, x_r, x_s, x_t, x_u, 
x_v, x_w, x_x, x_y, x_z, x_10, x_11, x_12, x_13, x_14, x_15, x_16, x_17, x_18, x_19, 
x_1a, x_1b, x_1c, x_1d, x_1e, x_1f, x_1g, x_1h, x_1i, x_1j, x_1k, x_1l, x_1m, x_1n, x_1o, 
x_1p, x_1q, x_1r, x_1s, x_1t, x_1u, x_1v, x_1w, x_1x, x_1y, x_1z, x_20, x_21, x_22, x_23, 
x_24, x_25, x_26, x_27, x_28, x_29, x_2a, x_2b, x_2c, x_2d, x_2e, x_2f, x_2g, x_2h, x_2i, 
x_2j, x_2k, x_2l, x_2m, x_2n, x_2o, x_2p, x_2q, x_2r, x_2s, x_2t, x_2u, x_2v, x_2w, x_2x, 
x_2y, x_2z, x_30, x_31, x_32, x_33, x_34, x_35, x_36, x_37, x_38, x_39, x_3a, x_3b, x_3c, 
x_3d, x_3e, x_3f, x_3g, x_3h, x_3i, x_3j, x_3k, x_3l, x_3m, x_3n, x_3o, x_3p, x_3q, x_3r, 
x_3s, x_3t, x_3u, x_3v, x_3w, x_3x, x_3y, x_3z, x_40, x_41, x_42, x_43, x_44, x_45, x_46, 
x_47, x_48, x_49, x_4a, x_4b, x_4c, x_4d, x_4e, x_4f, x_4g, x_4h, x_4i, x_4j, x_4k, x_4l, 
x_4m, x_4n, x_4o, x_4p, x_4q, x_4r, x_4s, x_4t, x_4u, x_4v, x_4w, x_4x, x_4y, x_4z, x_50, 
x_51, x_52, x_53, x_54, x_55, x_56, x_57, x_58, x_59, x_5a, x_5b, x_5c, x_5d, x_5e, x_5f, 
x_5g, x_5h, x_5i, x_5j, x_5k, x_5l, x_5m, x_5n, x_5o, x_5p, x_5q, x_5r, x_5s, x_5t, x_5u, 
x_5v, x_5w, x_5x, x_5y, x_5z, x_60, x_61, x_62, x_63, x_64, x_65, x_66, x_67, x_68, x_69, 
x_6a, x_6b, x_6c, x_6d, x_6e, x_6f, x_6g, x_6h, x_6i, x_6j, x_6k, x_6l, x_6m, x_6n, x_6o, 
x_6p, x_6q, x_6r, x_6s, x_6t, x_6u, x_6v, x_6w, x_6x, x_6y, x_6z, x_70, x_71, x_72, x_73, 
x_74, x_75, x_76, x_77, x_78, x_79, x_7a, x_7b, x_7c, x_7d, x_7e, x_7f, x_7g, x_7h, x_7i, 
x_7j, x_7k, x_7l, x_7m, x_7n, x_7o, x_7p, x_7q, x_7r, x_7s, x_7t, x_7u, x_7v, x_7w, x_7x, 
x_7y, x_7z, x_80, x_81, x_82, x_83, x_84, x_85, x_86, x_87, x_88, x_89, x_8a, x_8b, x_8c, 
x_8d, x_8e, x_8f, x_8g, x_8h, x_8i, x_8j, x_8k, x_8l, x_8m, x_8n, x_8o, x_8p, x_8q, x_8r, 
x_8s, x_8t, x_8u, x_8v, x_8w, x_8x, x_8y, x_8z, x_90, x_91, x_92, x_93, x_94, x_95, x_96, 
x_97, x_98, x_99, x_9a, x_9b, x_9c, x_9d, x_9e, x_9f, x_9g, x_9h, x_9i, x_9j, x_9k, x_9l, 
x_9m, x_9n, x_9o, x_9p, x_9q, x_9r, x_9s, x_9t, x_9u, x_9v, x_9w, x_9x, x_9y, x_9z, x_a0, 
x_a1, x_a2, x_a3, x_a4, x_a5, x_a6, x_a7, x_a8, x_a9, x_aa, x_ab, x_ac, x_ad, x_ae, x_af, 
x_ag, x_ah, x_ai, x_aj, x_ak, x_al, x_am, x_an, x_ao, x_ap, x_aq, x_ar, x_as, x_at, x_au, 
x_av, x_aw, x_ax, x_ay, x_az, x_b0, x_b1, x_b2, x_b3, x_b4, x_b5, x_b6, x_b7, x_b8, x_b9, 
x_ba, x_bb, x_bc, x_bd, x_be, x_bf, x_bg, x_bh, x_bi, x_bj, x_bk, x_bl, x_bm, x_bn, x_bo, 
x_bp, x_bq, x_br, x_bs, x_bt, x_bu, x_bv, x_bw, x_bx, x_by, x_bz, x_c0, x_c1, x_c2, x_c3, 
x_c4, x_c5, x_c6, x_c7, x_c8, x_c9, x_ca, x_cb, x_cc, x_cd, x_ce, x_cf, x_cg, x_ch, x_ci, 
x_cj, x_ck, x_cl, x_cm, x_cn, x_co, x_cp, x_cq, x_cr, x_cs, x_ct, x_cu, x_cv, x_cw, x_cx, 
x_cy, x_cz, x_d0, x_d1, x_d2, x_d3, x_d4, x_d5, x_d6, x_d7, x_d8, x_d9, x_da, x_db, x_dc, 
x_dd, x_de, x_df, x_dg, x_dh, x_di, x_dj, x_dk, x_dl, x_dm, x_dn, x_do, x_dp, x_dq, x_dr, 
x_ds, x_dt, x_du, x_dv, x_dw, x_dx, x_dy, x_dz, x_e0, x_e1, x_e2, x_e3, x_e4, x_e5, x_e6, 
x_e7, x_e8, x_e9, x_ea, x_eb, x_ec, x_ed, x_ee, x_ef, x_eg, x_eh, x_ei, x_ej, x_ek, x_el, 
x_em, x_en, x_eo, x_ep, x_eq, x_er, x_es, x_et, x_eu, x_ev, x_ew, x_ex, x_ey, x_ez, x_f0, 
x_f1, x_f2, x_f3, x_f4, x_f5, x_f6, x_f7, x_f8, x_f9, x_fa, x_fb, x_fc, x_fd, x_fe, x_ff, 
x_fg, x_fh, x_fi, x_fj, x_fk, x_fl, x_fm, x_fn, x_fo, x_fp, x_fq, x_fr, x_fs, x_ft, x_fu, 
x_fv, x_fw, x_fx, x_fy, x_fz, x_g0, x_g1, x_g2, x_g3, x_g4, x_g5, x_g6, x_g7, x_g8, x_g9, 
x_ga, x_gb, x_gc, x_gd, x_ge, x_gf, x_gg, x_gh, x_gi, x_gj, x_gk, x_gl, x_gm, x_gn, x_go, 
x_gp, x_gq, x_gr, x_gs, x_gt, x_gu, x_gv, x_gw, x_gx, x_gy, x_gz, x_h0, x_h1, x_h2, x_h3, 
x_h4, x_h5, x_h6, x_h7, x_h8, x_h9, x_ha, x_hb, x_hc, x_hd, x_he, x_hf, x_hg, x_hh, x_hi, 
x_hj, x_hk, x_hl, x_hm, x_hn, x_ho, x_hp, x_hq, x_hr, x_hs, x_ht, x_hu, x_hv, x_hw, x_hx, 
x_hy, x_hz, x_i0, x_i1, x_i2, x_i3, x_i4, x_i5, x_i6, x_i7, x_i8, x_i9, x_ia, x_ib, x_ic, 
x_id, x_ie, x_if, x_ig, x_ih, x_ii, x_ij, x_ik, x_il, x_im, x_in, x_io, x_ip, x_iq, x_ir, 
x_is, x_it, x_iu, x_iv, x_iw, x_ix, x_iy, x_iz, x_j0, x_j1, x_j2, x_j3, x_j4, x_j5, x_j6, 
x_j7, x_j8, x_j9, x_ja, x_jb, x_jc, x_jd, x_je, x_jf, x_jg, x_jh, x_ji, x_jj, x_jk, x_jl, 
x_jm, x_jn, x_jo, x_jp, x_jq, x_jr, x_js, x_jt, x_ju, x_jv, x_jw, x_jx, x_jy, x_jz, x_k0, 
x_k1, x_k2, x_k3, x_k4, x_k5, x_k6, x_k7, x_k8, x_k9, x_ka, x_kb, x_kc, x_kd, x_ke, x_kf, 
x_kg, x_kh, x_ki, x_kj, x_kk, x_kl, x_km, x_kn, x_ko, x_kp, x_kq, x_kr, x_ks, x_kt, x_ku, 
x_kv, x_kw, x_kx, x_ky, x_kz, x_l0, x_l1, x_l2, x_l3, x_l4, x_l5, x_l6, x_l7, x_l8, x_l9, 
x_la, x_lb, x_lc, x_ld, x_le, x_lf, x_lg, x_lh, x_li, x_lj, x_lk, x_ll, x_lm, x_ln, x_lo, 
x_lp, x_lq, x_lr, x_ls, x_lt, x_lu, x_lv, x_lw, x_lx, x_ly, x_lz, x_m0, x_m1, x_m2, x_m3, 
x_m4, x_m5, x_m6, x_m7, x_m8, x_m9, x_ma, x_mb, x_mc, x_md, x_me, x_mf, x_mg, x_mh, x_mi, 
x_mj, x_mk, x_ml, x_mm, x_mn, x_mo, x_mp, x_mq, x_mr, x_ms, x_mt, x_mu, x_mv, x_mw, x_mx, 
x_my, x_mz, x_n0, x_n1, x_n2, x_n3, x_n4, x_n5, x_n6, x_n7, x_n8, x_n9, x_na, x_nb, x_nc, 
x_nd, x_ne, x_nf, x_ng, x_nh, x_ni, x_nj, x_nk, x_nl, x_nm, x_nn, x_no, x_np, x_nq, x_nr, 
x_ns, x_nt, x_nu, x_nv, x_nw, x_nx, x_ny, x_nz, x_o0, x_o1, x_o2, x_o3, x_o4, x_o5, x_o6, 
x_o7, x_o8, x_o9, x_oa, x_ob, x_oc, x_od, x_oe, x_of, x_og, x_oh, x_oi, x_oj, x_ok, x_ol, 
x_om, x_on, x_oo, x_op, x_oq, x_or, x_os, x_ot, x_ou, x_ov, x_ow, x_ox, x_oy, x_oz, x_p0, 
x_p1, x_p2, x_p3, x_p4, x_p5, x_p6, x_p7, x_p8, x_p9, x_pa, x_pb, x_pc, x_pd, x_pe, x_pf, 
x_pg, x_ph, x_pi, x_pj, x_pk, x_pl, x_pm, x_pn, x_po, x_pp, x_pq, x_pr, x_ps, x_pt, x_pu, 
x_pv, x_pw, x_px, x_py, x_pz, x_q0, x_q1, x_q2, x_q3, x_q4, x_q5, x_q6, x_q7, x_q8, x_q9, 
x_qa, x_qb, x_qc, x_qd, x_qe, x_qf, x_qg, x_qh, x_qi, x_qj, x_qk, x_ql, x_qm, x_qn, x_qo, 
x_qp, x_qq, x_qr, x_qs, x_qt, x_qu, x_qv, x_qw, x_qx, x_qy, x_qz, x_r0, x_r1, x_r2, x_r3, 
x_r4, x_r5, x_r6, x_r7, x_r8, x_r9, x_ra, x_rb, x_rc, x_rd, x_re, x_rf, x_rg, x_rh, x_ri, 
x_rj, x_rk, x_rl, x_rm, x_rn, x_ro, x_rp, x_rq, x_rr, x_rs, x_rt, x_ru, x_rv, x_rw, x_rx, 
x_ry, x_rz, x_s0, x_s1, x_s2, x_s3, x_s4, x_s5, x_s6, x_s7, x_s8, x_s9, dummy;
x_1 = 0;
x_2 = text << 57ULL;
x_3 = x_2 & 0x8000000000000000ULL;
x_1 |= x_3;
x_4 = text << 48ULL;
x_5 = x_4 & 0x4000000000000000ULL;
x_1 |= x_5;
x_6 = text << 39ULL;
x_7 = x_6 & 0x2000000000000000ULL;
x_1 |= x_7;
x_8 = text << 30ULL;
x_9 = x_8 & 0x1000000000000000ULL;
x_1 |= x_9;
x_a = text << 21ULL;
x_b = x_a & 0x800000000000000ULL;
x_1 |= x_b;
x_c = text << 12ULL;
x_d = x_c & 0x400000000000000ULL;
x_1 |= x_d;
x_e = text << 3ULL;
x_f = x_e & 0x200000000000000ULL;
x_1 |= x_f;
x_g = text >> 6ULL;
x_h = x_g & 0x100000000000000ULL;
x_1 |= x_h;
x_i = text << 51ULL;
x_j = x_i & 0x80000000000000ULL;
x_1 |= x_j;
x_k = text << 42ULL;
x_l = x_k & 0x40000000000000ULL;
x_1 |= x_l;
x_m = text << 33ULL;
x_n = x_m & 0x20000000000000ULL;
x_1 |= x_n;
x_o = text << 24ULL;
x_p = x_o & 0x10000000000000ULL;
x_1 |= x_p;
x_q = text << 15ULL;
x_r = x_q & 0x8000000000000ULL;
x_1 |= x_r;
x_s = text << 6ULL;
x_t = x_s & 0x4000000000000ULL;
x_1 |= x_t;
x_u = text >> 3ULL;
x_v = x_u & 0x2000000000000ULL;
x_1 |= x_v;
x_w = text >> 12ULL;
x_x = x_w & 0x1000000000000ULL;
x_1 |= x_x;
x_y = text << 45ULL;
x_z = x_y & 0x800000000000ULL;
x_1 |= x_z;
x_10 = text << 36ULL;
x_11 = x_10 & 0x400000000000ULL;
x_1 |= x_11;
x_12 = text << 27ULL;
x_13 = x_12 & 0x200000000000ULL;
x_1 |= x_13;
x_14 = text << 18ULL;
x_15 = x_14 & 0x100000000000ULL;
x_1 |= x_15;
x_16 = text << 9ULL;
x_17 = x_16 & 0x80000000000ULL;
x_1 |= x_17;
x_18 = text << 0ULL;
x_19 = x_18 & 0x40000000000ULL;
x_1 |= x_19;
x_1a = text >> 9ULL;
x_1b = x_1a & 0x20000000000ULL;
x_1 |= x_1b;
x_1c = text >> 18ULL;
x_1d = x_1c & 0x10000000000ULL;
x_1 |= x_1d;
x_1e = text << 39ULL;
x_1f = x_1e & 0x8000000000ULL;
x_1 |= x_1f;
x_1g = text << 30ULL;
x_1h = x_1g & 0x4000000000ULL;
x_1 |= x_1h;
x_1i = text << 21ULL;
x_1j = x_1i & 0x2000000000ULL;
x_1 |= x_1j;
x_1k = text << 12ULL;
x_1l = x_1k & 0x1000000000ULL;
x_1 |= x_1l;
x_1m = text << 3ULL;
x_1n = x_1m & 0x800000000ULL;
x_1 |= x_1n;
x_1o = text >> 6ULL;
x_1p = x_1o & 0x400000000ULL;
x_1 |= x_1p;
x_1q = text >> 15ULL;
x_1r = x_1q & 0x200000000ULL;
x_1 |= x_1r;
x_1s = text >> 24ULL;
x_1t = x_1s & 0x100000000ULL;
x_1 |= x_1t;
x_1u = text << 24ULL;
x_1v = x_1u & 0x80000000ULL;
x_1 |= x_1v;
x_1w = text << 15ULL;
x_1x = x_1w & 0x40000000ULL;
x_1 |= x_1x;
x_1y = text << 6ULL;
x_1z = x_1y & 0x20000000ULL;
x_1 |= x_1z;
x_20 = text >> 3ULL;
x_21 = x_20 & 0x10000000ULL;
x_1 |= x_21;
x_22 = text >> 12ULL;
x_23 = x_22 & 0x8000000ULL;
x_1 |= x_23;
x_24 = text >> 21ULL;
x_25 = x_24 & 0x4000000ULL;
x_1 |= x_25;
x_26 = text >> 30ULL;
x_27 = x_26 & 0x2000000ULL;
x_1 |= x_27;
x_28 = text >> 39ULL;
x_29 = x_28 & 0x1000000ULL;
x_1 |= x_29;
x_2a = text << 18ULL;
x_2b = x_2a & 0x800000ULL;
x_1 |= x_2b;
x_2c = text << 9ULL;
x_2d = x_2c & 0x400000ULL;
x_1 |= x_2d;
x_2e = text << 0ULL;
x_2f = x_2e & 0x200000ULL;
x_1 |= x_2f;
x_2g = text >> 9ULL;
x_2h = x_2g & 0x100000ULL;
x_1 |= x_2h;
x_2i = text >> 18ULL;
x_2j = x_2i & 0x80000ULL;
x_1 |= x_2j;
x_2k = text >> 27ULL;
x_2l = x_2k & 0x40000ULL;
x_1 |= x_2l;
x_2m = text >> 36ULL;
x_2n = x_2m & 0x20000ULL;
x_1 |= x_2n;
x_2o = text >> 45ULL;
x_2p = x_2o & 0x10000ULL;
x_1 |= x_2p;
x_2q = text << 12ULL;
x_2r = x_2q & 0x8000ULL;
x_1 |= x_2r;
x_2s = text << 3ULL;
x_2t = x_2s & 0x4000ULL;
x_1 |= x_2t;
x_2u = text >> 6ULL;
x_2v = x_2u & 0x2000ULL;
x_1 |= x_2v;
x_2w = text >> 15ULL;
x_2x = x_2w & 0x1000ULL;
x_1 |= x_2x;
x_2y = text >> 24ULL;
x_2z = x_2y & 0x800ULL;
x_1 |= x_2z;
x_30 = text >> 33ULL;
x_31 = x_30 & 0x400ULL;
x_1 |= x_31;
x_32 = text >> 42ULL;
x_33 = x_32 & 0x200ULL;
x_1 |= x_33;
x_34 = text >> 51ULL;
x_35 = x_34 & 0x100ULL;
x_1 |= x_35;
x_36 = text << 6ULL;
x_37 = x_36 & 0x80ULL;
x_1 |= x_37;
x_38 = text >> 3ULL;
x_39 = x_38 & 0x40ULL;
x_1 |= x_39;
x_3a = text >> 12ULL;
x_3b = x_3a & 0x20ULL;
x_1 |= x_3b;
x_3c = text >> 21ULL;
x_3d = x_3c & 0x10ULL;
x_1 |= x_3d;
x_3e = text >> 30ULL;
x_3f = x_3e & 0x8ULL;
x_1 |= x_3f;
x_3g = text >> 39ULL;
x_3h = x_3g & 0x4ULL;
x_1 |= x_3h;
x_3i = text >> 48ULL;
x_3j = x_3i & 0x2ULL;
x_1 |= x_3j;
x_3k = text >> 57ULL;
x_3l = x_3k & 0x1ULL;
x_1 |= x_3l;
x_3m = x_1 >> 32ULL;
x_3n = x_3m & 4294967295ULL;
x_3o = x_1 & 4294967295ULL;
x_3p = 0;
x_3q = x_3o & 281474976710655ULL;
x_3r = x_3q << 47ULL;
x_3s = x_3q << 15ULL;
x_3t = x_3s & 0x7c0000000000ULL;
x_3u = x_3q << 13ULL;
x_3v = x_3u & 0x3f000000000ULL;
x_3w = x_3q << 11ULL;
x_3x = x_3w & 0xfc0000000ULL;
x_3y = x_3q << 9ULL;
x_3z = x_3y & 0x3f000000ULL;
x_40 = x_3q << 7ULL;
x_41 = x_40 & 0xfc0000ULL;
x_42 = x_3q << 5ULL;
x_43 = x_42 & 0x3f000ULL;
x_44 = x_3q << 3ULL;
x_45 = x_44 & 0xfc0ULL;
x_46 = x_3q << 1ULL;
x_47 = x_46 & 0x3eULL;
x_48 = x_3q >> 31ULL;
x_49 = x_3r;
x_49 |= x_3t;
x_49 |= x_3v;
x_49 |= x_3x;
x_49 |= x_3z;
x_49 |= x_41;
x_49 |= x_43;
x_49 |= x_45;
x_49 |= x_47;
x_49 |= x_48;
x_4a = x_49 ^ key_0;
x_4b = x_4a >> 42ULL;
x_4c = x_4b & 0x3fULL;
x_4d = s_box_0[x_4c];
x_3p ^= x_4d;
x_4e = x_4a >> 36ULL;
x_4f = x_4e & 0x3fULL;
x_4g = s_box_1[x_4f];
x_3p ^= x_4g;
x_4h = x_4a >> 30ULL;
x_4i = x_4h & 0x3fULL;
x_4j = s_box_2[x_4i];
x_3p ^= x_4j;
x_4k = x_4a >> 24ULL;
x_4l = x_4k & 0x3fULL;
x_4m = s_box_3[x_4l];
x_3p ^= x_4m;
x_4n = x_4a >> 18ULL;
x_4o = x_4n & 0x3fULL;
x_4p = s_box_4[x_4o];
x_3p ^= x_4p;
x_4q = x_4a >> 12ULL;
x_4r = x_4q & 0x3fULL;
x_4s = s_box_5[x_4r];
x_3p ^= x_4s;
x_4t = x_4a >> 6ULL;
x_4u = x_4t & 0x3fULL;
x_4v = s_box_6[x_4u];
x_3p ^= x_4v;
x_4w = x_4a >> 0ULL;
x_4x = x_4w & 0x3fULL;
x_4y = s_box_7[x_4x];
x_3p ^= x_4y;
x_4z = x_3n ^ x_3p;
x_50 = 0;
x_51 = x_4z & 281474976710655ULL;
x_52 = x_51 << 47ULL;
x_53 = x_51 << 15ULL;
x_54 = x_53 & 0x7c0000000000ULL;
x_55 = x_51 << 13ULL;
x_56 = x_55 & 0x3f000000000ULL;
x_57 = x_51 << 11ULL;
x_58 = x_57 & 0xfc0000000ULL;
x_59 = x_51 << 9ULL;
x_5a = x_59 & 0x3f000000ULL;
x_5b = x_51 << 7ULL;
x_5c = x_5b & 0xfc0000ULL;
x_5d = x_51 << 5ULL;
x_5e = x_5d & 0x3f000ULL;
x_5f = x_51 << 3ULL;
x_5g = x_5f & 0xfc0ULL;
x_5h = x_51 << 1ULL;
x_5i = x_5h & 0x3eULL;
x_5j = x_51 >> 31ULL;
x_5k = x_52;
x_5k |= x_54;
x_5k |= x_56;
x_5k |= x_58;
x_5k |= x_5a;
x_5k |= x_5c;
x_5k |= x_5e;
x_5k |= x_5g;
x_5k |= x_5i;
x_5k |= x_5j;
x_5l = x_5k ^ key_1;
x_5m = x_5l >> 42ULL;
x_5n = x_5m & 0x3fULL;
x_5o = s_box_0[x_5n];
x_50 ^= x_5o;
x_5p = x_5l >> 36ULL;
x_5q = x_5p & 0x3fULL;
x_5r = s_box_1[x_5q];
x_50 ^= x_5r;
x_5s = x_5l >> 30ULL;
x_5t = x_5s & 0x3fULL;
x_5u = s_box_2[x_5t];
x_50 ^= x_5u;
x_5v = x_5l >> 24ULL;
x_5w = x_5v & 0x3fULL;
x_5x = s_box_3[x_5w];
x_50 ^= x_5x;
x_5y = x_5l >> 18ULL;
x_5z = x_5y & 0x3fULL;
x_60 = s_box_4[x_5z];
x_50 ^= x_60;
x_61 = x_5l >> 12ULL;
x_62 = x_61 & 0x3fULL;
x_63 = s_box_5[x_62];
x_50 ^= x_63;
x_64 = x_5l >> 6ULL;
x_65 = x_64 & 0x3fULL;
x_66 = s_box_6[x_65];
x_50 ^= x_66;
x_67 = x_5l >> 0ULL;
x_68 = x_67 & 0x3fULL;
x_69 = s_box_7[x_68];
x_50 ^= x_69;
x_6a = x_3o ^ x_50;
x_6b = 0;
x_6c = x_6a & 281474976710655ULL;
x_6d = x_6c << 47ULL;
x_6e = x_6c << 15ULL;
x_6f = x_6e & 0x7c0000000000ULL;
x_6g = x_6c << 13ULL;
x_6h = x_6g & 0x3f000000000ULL;
x_6i = x_6c << 11ULL;
x_6j = x_6i & 0xfc0000000ULL;
x_6k = x_6c << 9ULL;
x_6l = x_6k & 0x3f000000ULL;
x_6m = x_6c << 7ULL;
x_6n = x_6m & 0xfc0000ULL;
x_6o = x_6c << 5ULL;
x_6p = x_6o & 0x3f000ULL;
x_6q = x_6c << 3ULL;
x_6r = x_6q & 0xfc0ULL;
x_6s = x_6c << 1ULL;
x_6t = x_6s & 0x3eULL;
x_6u = x_6c >> 31ULL;
x_6v = x_6d;
x_6v |= x_6f;
x_6v |= x_6h;
x_6v |= x_6j;
x_6v |= x_6l;
x_6v |= x_6n;
x_6v |= x_6p;
x_6v |= x_6r;
x_6v |= x_6t;
x_6v |= x_6u;
x_6w = x_6v ^ key_2;
x_6x = x_6w >> 42ULL;
x_6y = x_6x & 0x3fULL;
x_6z = s_box_0[x_6y];
x_6b ^= x_6z;
x_70 = x_6w >> 36ULL;
x_71 = x_70 & 0x3fULL;
x_72 = s_box_1[x_71];
x_6b ^= x_72;
x_73 = x_6w >> 30ULL;
x_74 = x_73 & 0x3fULL;
x_75 = s_box_2[x_74];
x_6b ^= x_75;
x_76 = x_6w >> 24ULL;
x_77 = x_76 & 0x3fULL;
x_78 = s_box_3[x_77];
x_6b ^= x_78;
x_79 = x_6w >> 18ULL;
x_7a = x_79 & 0x3fULL;
x_7b = s_box_4[x_7a];
x_6b ^= x_7b;
x_7c = x_6w >> 12ULL;
x_7d = x_7c & 0x3fULL;
x_7e = s_box_5[x_7d];
x_6b ^= x_7e;
x_7f = x_6w >> 6ULL;
x_7g = x_7f & 0x3fULL;
x_7h = s_box_6[x_7g];
x_6b ^= x_7h;
x_7i = x_6w >> 0ULL;
x_7j = x_7i & 0x3fULL;
x_7k = s_box_7[x_7j];
x_6b ^= x_7k;
x_7l = x_4z ^ x_6b;
x_7m = 0;
x_7n = x_7l & 281474976710655ULL;
x_7o = x_7n << 47ULL;
x_7p = x_7n << 15ULL;
x_7q = x_7p & 0x7c0000000000ULL;
x_7r = x_7n << 13ULL;
x_7s = x_7r & 0x3f000000000ULL;
x_7t = x_7n << 11ULL;
x_7u = x_7t & 0xfc0000000ULL;
x_7v = x_7n << 9ULL;
x_7w = x_7v & 0x3f000000ULL;
x_7x = x_7n << 7ULL;
x_7y = x_7x & 0xfc0000ULL;
x_7z = x_7n << 5ULL;
x_80 = x_7z & 0x3f000ULL;
x_81 = x_7n << 3ULL;
x_82 = x_81 & 0xfc0ULL;
x_83 = x_7n << 1ULL;
x_84 = x_83 & 0x3eULL;
x_85 = x_7n >> 31ULL;
x_86 = x_7o;
x_86 |= x_7q;
x_86 |= x_7s;
x_86 |= x_7u;
x_86 |= x_7w;
x_86 |= x_7y;
x_86 |= x_80;
x_86 |= x_82;
x_86 |= x_84;
x_86 |= x_85;
x_87 = x_86 ^ key_3;
x_88 = x_87 >> 42ULL;
x_89 = x_88 & 0x3fULL;
x_8a = s_box_0[x_89];
x_7m ^= x_8a;
x_8b = x_87 >> 36ULL;
x_8c = x_8b & 0x3fULL;
x_8d = s_box_1[x_8c];
x_7m ^= x_8d;
x_8e = x_87 >> 30ULL;
x_8f = x_8e & 0x3fULL;
x_8g = s_box_2[x_8f];
x_7m ^= x_8g;
x_8h = x_87 >> 24ULL;
x_8i = x_8h & 0x3fULL;
x_8j = s_box_3[x_8i];
x_7m ^= x_8j;
x_8k = x_87 >> 18ULL;
x_8l = x_8k & 0x3fULL;
x_8m = s_box_4[x_8l];
x_7m ^= x_8m;
x_8n = x_87 >> 12ULL;
x_8o = x_8n & 0x3fULL;
x_8p = s_box_5[x_8o];
x_7m ^= x_8p;
x_8q = x_87 >> 6ULL;
x_8r = x_8q & 0x3fULL;
x_8s = s_box_6[x_8r];
x_7m ^= x_8s;
x_8t = x_87 >> 0ULL;
x_8u = x_8t & 0x3fULL;
x_8v = s_box_7[x_8u];
x_7m ^= x_8v;
x_8w = x_6a ^ x_7m;
x_8x = 0;
x_8y = x_8w & 281474976710655ULL;
x_8z = x_8y << 47ULL;
x_90 = x_8y << 15ULL;
x_91 = x_90 & 0x7c0000000000ULL;
x_92 = x_8y << 13ULL;
x_93 = x_92 & 0x3f000000000ULL;
x_94 = x_8y << 11ULL;
x_95 = x_94 & 0xfc0000000ULL;
x_96 = x_8y << 9ULL;
x_97 = x_96 & 0x3f000000ULL;
x_98 = x_8y << 7ULL;
x_99 = x_98 & 0xfc0000ULL;
x_9a = x_8y << 5ULL;
x_9b = x_9a & 0x3f000ULL;
x_9c = x_8y << 3ULL;
x_9d = x_9c & 0xfc0ULL;
x_9e = x_8y << 1ULL;
x_9f = x_9e & 0x3eULL;
x_9g = x_8y >> 31ULL;
x_9h = x_8z;
x_9h |= x_91;
x_9h |= x_93;
x_9h |= x_95;
x_9h |= x_97;
x_9h |= x_99;
x_9h |= x_9b;
x_9h |= x_9d;
x_9h |= x_9f;
x_9h |= x_9g;
x_9i = x_9h ^ key_4;
x_9j = x_9i >> 42ULL;
x_9k = x_9j & 0x3fULL;
x_9l = s_box_0[x_9k];
x_8x ^= x_9l;
x_9m = x_9i >> 36ULL;
x_9n = x_9m & 0x3fULL;
x_9o = s_box_1[x_9n];
x_8x ^= x_9o;
x_9p = x_9i >> 30ULL;
x_9q = x_9p & 0x3fULL;
x_9r = s_box_2[x_9q];
x_8x ^= x_9r;
x_9s = x_9i >> 24ULL;
x_9t = x_9s & 0x3fULL;
x_9u = s_box_3[x_9t];
x_8x ^= x_9u;
x_9v = x_9i >> 18ULL;
x_9w = x_9v & 0x3fULL;
x_9x = s_box_4[x_9w];
x_8x ^= x_9x;
x_9y = x_9i >> 12ULL;
x_9z = x_9y & 0x3fULL;
x_a0 = s_box_5[x_9z];
x_8x ^= x_a0;
x_a1 = x_9i >> 6ULL;
x_a2 = x_a1 & 0x3fULL;
x_a3 = s_box_6[x_a2];
x_8x ^= x_a3;
x_a4 = x_9i >> 0ULL;
x_a5 = x_a4 & 0x3fULL;
x_a6 = s_box_7[x_a5];
x_8x ^= x_a6;
x_a7 = x_7l ^ x_8x;
x_a8 = 0;
x_a9 = x_a7 & 281474976710655ULL;
x_aa = x_a9 << 47ULL;
x_ab = x_a9 << 15ULL;
x_ac = x_ab & 0x7c0000000000ULL;
x_ad = x_a9 << 13ULL;
x_ae = x_ad & 0x3f000000000ULL;
x_af = x_a9 << 11ULL;
x_ag = x_af & 0xfc0000000ULL;
x_ah = x_a9 << 9ULL;
x_ai = x_ah & 0x3f000000ULL;
x_aj = x_a9 << 7ULL;
x_ak = x_aj & 0xfc0000ULL;
x_al = x_a9 << 5ULL;
x_am = x_al & 0x3f000ULL;
x_an = x_a9 << 3ULL;
x_ao = x_an & 0xfc0ULL;
x_ap = x_a9 << 1ULL;
x_aq = x_ap & 0x3eULL;
x_ar = x_a9 >> 31ULL;
x_as = x_aa;
x_as |= x_ac;
x_as |= x_ae;
x_as |= x_ag;
x_as |= x_ai;
x_as |= x_ak;
x_as |= x_am;
x_as |= x_ao;
x_as |= x_aq;
x_as |= x_ar;
x_at = x_as ^ key_5;
x_au = x_at >> 42ULL;
x_av = x_au & 0x3fULL;
x_aw = s_box_0[x_av];
x_a8 ^= x_aw;
x_ax = x_at >> 36ULL;
x_ay = x_ax & 0x3fULL;
x_az = s_box_1[x_ay];
x_a8 ^= x_az;
x_b0 = x_at >> 30ULL;
x_b1 = x_b0 & 0x3fULL;
x_b2 = s_box_2[x_b1];
x_a8 ^= x_b2;
x_b3 = x_at >> 24ULL;
x_b4 = x_b3 & 0x3fULL;
x_b5 = s_box_3[x_b4];
x_a8 ^= x_b5;
x_b6 = x_at >> 18ULL;
x_b7 = x_b6 & 0x3fULL;
x_b8 = s_box_4[x_b7];
x_a8 ^= x_b8;
x_b9 = x_at >> 12ULL;
x_ba = x_b9 & 0x3fULL;
x_bb = s_box_5[x_ba];
x_a8 ^= x_bb;
x_bc = x_at >> 6ULL;
x_bd = x_bc & 0x3fULL;
x_be = s_box_6[x_bd];
x_a8 ^= x_be;
x_bf = x_at >> 0ULL;
x_bg = x_bf & 0x3fULL;
x_bh = s_box_7[x_bg];
x_a8 ^= x_bh;
x_bi = x_8w ^ x_a8;
x_bj = 0;
x_bk = x_bi & 281474976710655ULL;
x_bl = x_bk << 47ULL;
x_bm = x_bk << 15ULL;
x_bn = x_bm & 0x7c0000000000ULL;
x_bo = x_bk << 13ULL;
x_bp = x_bo & 0x3f000000000ULL;
x_bq = x_bk << 11ULL;
x_br = x_bq & 0xfc0000000ULL;
x_bs = x_bk << 9ULL;
x_bt = x_bs & 0x3f000000ULL;
x_bu = x_bk << 7ULL;
x_bv = x_bu & 0xfc0000ULL;
x_bw = x_bk << 5ULL;
x_bx = x_bw & 0x3f000ULL;
x_by = x_bk << 3ULL;
x_bz = x_by & 0xfc0ULL;
x_c0 = x_bk << 1ULL;
x_c1 = x_c0 & 0x3eULL;
x_c2 = x_bk >> 31ULL;
x_c3 = x_bl;
x_c3 |= x_bn;
x_c3 |= x_bp;
x_c3 |= x_br;
x_c3 |= x_bt;
x_c3 |= x_bv;
x_c3 |= x_bx;
x_c3 |= x_bz;
x_c3 |= x_c1;
x_c3 |= x_c2;
x_c4 = x_c3 ^ key_6;
x_c5 = x_c4 >> 42ULL;
x_c6 = x_c5 & 0x3fULL;
x_c7 = s_box_0[x_c6];
x_bj ^= x_c7;
x_c8 = x_c4 >> 36ULL;
x_c9 = x_c8 & 0x3fULL;
x_ca = s_box_1[x_c9];
x_bj ^= x_ca;
x_cb = x_c4 >> 30ULL;
x_cc = x_cb & 0x3fULL;
x_cd = s_box_2[x_cc];
x_bj ^= x_cd;
x_ce = x_c4 >> 24ULL;
x_cf = x_ce & 0x3fULL;
x_cg = s_box_3[x_cf];
x_bj ^= x_cg;
x_ch = x_c4 >> 18ULL;
x_ci = x_ch & 0x3fULL;
x_cj = s_box_4[x_ci];
x_bj ^= x_cj;
x_ck = x_c4 >> 12ULL;
x_cl = x_ck & 0x3fULL;
x_cm = s_box_5[x_cl];
x_bj ^= x_cm;
x_cn = x_c4 >> 6ULL;
x_co = x_cn & 0x3fULL;
x_cp = s_box_6[x_co];
x_bj ^= x_cp;
x_cq = x_c4 >> 0ULL;
x_cr = x_cq & 0x3fULL;
x_cs = s_box_7[x_cr];
x_bj ^= x_cs;
x_ct = x_a7 ^ x_bj;
x_cu = 0;
x_cv = x_ct & 281474976710655ULL;
x_cw = x_cv << 47ULL;
x_cx = x_cv << 15ULL;
x_cy = x_cx & 0x7c0000000000ULL;
x_cz = x_cv << 13ULL;
x_d0 = x_cz & 0x3f000000000ULL;
x_d1 = x_cv << 11ULL;
x_d2 = x_d1 & 0xfc0000000ULL;
x_d3 = x_cv << 9ULL;
x_d4 = x_d3 & 0x3f000000ULL;
x_d5 = x_cv << 7ULL;
x_d6 = x_d5 & 0xfc0000ULL;
x_d7 = x_cv << 5ULL;
x_d8 = x_d7 & 0x3f000ULL;
x_d9 = x_cv << 3ULL;
x_da = x_d9 & 0xfc0ULL;
x_db = x_cv << 1ULL;
x_dc = x_db & 0x3eULL;
x_dd = x_cv >> 31ULL;
x_de = x_cw;
x_de |= x_cy;
x_de |= x_d0;
x_de |= x_d2;
x_de |= x_d4;
x_de |= x_d6;
x_de |= x_d8;
x_de |= x_da;
x_de |= x_dc;
x_de |= x_dd;
x_df = x_de ^ key_7;
x_dg = x_df >> 42ULL;
x_dh = x_dg & 0x3fULL;
x_di = s_box_0[x_dh];
x_cu ^= x_di;
x_dj = x_df >> 36ULL;
x_dk = x_dj & 0x3fULL;
x_dl = s_box_1[x_dk];
x_cu ^= x_dl;
x_dm = x_df >> 30ULL;
x_dn = x_dm & 0x3fULL;
x_do = s_box_2[x_dn];
x_cu ^= x_do;
x_dp = x_df >> 24ULL;
x_dq = x_dp & 0x3fULL;
x_dr = s_box_3[x_dq];
x_cu ^= x_dr;
x_ds = x_df >> 18ULL;
x_dt = x_ds & 0x3fULL;
x_du = s_box_4[x_dt];
x_cu ^= x_du;
x_dv = x_df >> 12ULL;
x_dw = x_dv & 0x3fULL;
x_dx = s_box_5[x_dw];
x_cu ^= x_dx;
x_dy = x_df >> 6ULL;
x_dz = x_dy & 0x3fULL;
x_e0 = s_box_6[x_dz];
x_cu ^= x_e0;
x_e1 = x_df >> 0ULL;
x_e2 = x_e1 & 0x3fULL;
x_e3 = s_box_7[x_e2];
x_cu ^= x_e3;
x_e4 = x_bi ^ x_cu;
x_e5 = 0;
x_e6 = x_e4 & 281474976710655ULL;
x_e7 = x_e6 << 47ULL;
x_e8 = x_e6 << 15ULL;
x_e9 = x_e8 & 0x7c0000000000ULL;
x_ea = x_e6 << 13ULL;
x_eb = x_ea & 0x3f000000000ULL;
x_ec = x_e6 << 11ULL;
x_ed = x_ec & 0xfc0000000ULL;
x_ee = x_e6 << 9ULL;
x_ef = x_ee & 0x3f000000ULL;
x_eg = x_e6 << 7ULL;
x_eh = x_eg & 0xfc0000ULL;
x_ei = x_e6 << 5ULL;
x_ej = x_ei & 0x3f000ULL;
x_ek = x_e6 << 3ULL;
x_el = x_ek & 0xfc0ULL;
x_em = x_e6 << 1ULL;
x_en = x_em & 0x3eULL;
x_eo = x_e6 >> 31ULL;
x_ep = x_e7;
x_ep |= x_e9;
x_ep |= x_eb;
x_ep |= x_ed;
x_ep |= x_ef;
x_ep |= x_eh;
x_ep |= x_ej;
x_ep |= x_el;
x_ep |= x_en;
x_ep |= x_eo;
x_eq = x_ep ^ key_8;
x_er = x_eq >> 42ULL;
x_es = x_er & 0x3fULL;
x_et = s_box_0[x_es];
x_e5 ^= x_et;
x_eu = x_eq >> 36ULL;
x_ev = x_eu & 0x3fULL;
x_ew = s_box_1[x_ev];
x_e5 ^= x_ew;
x_ex = x_eq >> 30ULL;
x_ey = x_ex & 0x3fULL;
x_ez = s_box_2[x_ey];
x_e5 ^= x_ez;
x_f0 = x_eq >> 24ULL;
x_f1 = x_f0 & 0x3fULL;
x_f2 = s_box_3[x_f1];
x_e5 ^= x_f2;
x_f3 = x_eq >> 18ULL;
x_f4 = x_f3 & 0x3fULL;
x_f5 = s_box_4[x_f4];
x_e5 ^= x_f5;
x_f6 = x_eq >> 12ULL;
x_f7 = x_f6 & 0x3fULL;
x_f8 = s_box_5[x_f7];
x_e5 ^= x_f8;
x_f9 = x_eq >> 6ULL;
x_fa = x_f9 & 0x3fULL;
x_fb = s_box_6[x_fa];
x_e5 ^= x_fb;
x_fc = x_eq >> 0ULL;
x_fd = x_fc & 0x3fULL;
x_fe = s_box_7[x_fd];
x_e5 ^= x_fe;
x_ff = x_ct ^ x_e5;
x_fg = 0;
x_fh = x_ff & 281474976710655ULL;
x_fi = x_fh << 47ULL;
x_fj = x_fh << 15ULL;
x_fk = x_fj & 0x7c0000000000ULL;
x_fl = x_fh << 13ULL;
x_fm = x_fl & 0x3f000000000ULL;
x_fn = x_fh << 11ULL;
x_fo = x_fn & 0xfc0000000ULL;
x_fp = x_fh << 9ULL;
x_fq = x_fp & 0x3f000000ULL;
x_fr = x_fh << 7ULL;
x_fs = x_fr & 0xfc0000ULL;
x_ft = x_fh << 5ULL;
x_fu = x_ft & 0x3f000ULL;
x_fv = x_fh << 3ULL;
x_fw = x_fv & 0xfc0ULL;
x_fx = x_fh << 1ULL;
x_fy = x_fx & 0x3eULL;
x_fz = x_fh >> 31ULL;
x_g0 = x_fi;
x_g0 |= x_fk;
x_g0 |= x_fm;
x_g0 |= x_fo;
x_g0 |= x_fq;
x_g0 |= x_fs;
x_g0 |= x_fu;
x_g0 |= x_fw;
x_g0 |= x_fy;
x_g0 |= x_fz;
x_g1 = x_g0 ^ key_9;
x_g2 = x_g1 >> 42ULL;
x_g3 = x_g2 & 0x3fULL;
x_g4 = s_box_0[x_g3];
x_fg ^= x_g4;
x_g5 = x_g1 >> 36ULL;
x_g6 = x_g5 & 0x3fULL;
x_g7 = s_box_1[x_g6];
x_fg ^= x_g7;
x_g8 = x_g1 >> 30ULL;
x_g9 = x_g8 & 0x3fULL;
x_ga = s_box_2[x_g9];
x_fg ^= x_ga;
x_gb = x_g1 >> 24ULL;
x_gc = x_gb & 0x3fULL;
x_gd = s_box_3[x_gc];
x_fg ^= x_gd;
x_ge = x_g1 >> 18ULL;
x_gf = x_ge & 0x3fULL;
x_gg = s_box_4[x_gf];
x_fg ^= x_gg;
x_gh = x_g1 >> 12ULL;
x_gi = x_gh & 0x3fULL;
x_gj = s_box_5[x_gi];
x_fg ^= x_gj;
x_gk = x_g1 >> 6ULL;
x_gl = x_gk & 0x3fULL;
x_gm = s_box_6[x_gl];
x_fg ^= x_gm;
x_gn = x_g1 >> 0ULL;
x_go = x_gn & 0x3fULL;
x_gp = s_box_7[x_go];
x_fg ^= x_gp;
x_gq = x_e4 ^ x_fg;
x_gr = 0;
x_gs = x_gq & 281474976710655ULL;
x_gt = x_gs << 47ULL;
x_gu = x_gs << 15ULL;
x_gv = x_gu & 0x7c0000000000ULL;
x_gw = x_gs << 13ULL;
x_gx = x_gw & 0x3f000000000ULL;
x_gy = x_gs << 11ULL;
x_gz = x_gy & 0xfc0000000ULL;
x_h0 = x_gs << 9ULL;
x_h1 = x_h0 & 0x3f000000ULL;
x_h2 = x_gs << 7ULL;
x_h3 = x_h2 & 0xfc0000ULL;
x_h4 = x_gs << 5ULL;
x_h5 = x_h4 & 0x3f000ULL;
x_h6 = x_gs << 3ULL;
x_h7 = x_h6 & 0xfc0ULL;
x_h8 = x_gs << 1ULL;
x_h9 = x_h8 & 0x3eULL;
x_ha = x_gs >> 31ULL;
x_hb = x_gt;
x_hb |= x_gv;
x_hb |= x_gx;
x_hb |= x_gz;
x_hb |= x_h1;
x_hb |= x_h3;
x_hb |= x_h5;
x_hb |= x_h7;
x_hb |= x_h9;
x_hb |= x_ha;
x_hc = x_hb ^ key_10;
x_hd = x_hc >> 42ULL;
x_he = x_hd & 0x3fULL;
x_hf = s_box_0[x_he];
x_gr ^= x_hf;
x_hg = x_hc >> 36ULL;
x_hh = x_hg & 0x3fULL;
x_hi = s_box_1[x_hh];
x_gr ^= x_hi;
x_hj = x_hc >> 30ULL;
x_hk = x_hj & 0x3fULL;
x_hl = s_box_2[x_hk];
x_gr ^= x_hl;
x_hm = x_hc >> 24ULL;
x_hn = x_hm & 0x3fULL;
x_ho = s_box_3[x_hn];
x_gr ^= x_ho;
x_hp = x_hc >> 18ULL;
x_hq = x_hp & 0x3fULL;
x_hr = s_box_4[x_hq];
x_gr ^= x_hr;
x_hs = x_hc >> 12ULL;
x_ht = x_hs & 0x3fULL;
x_hu = s_box_5[x_ht];
x_gr ^= x_hu;
x_hv = x_hc >> 6ULL;
x_hw = x_hv & 0x3fULL;
x_hx = s_box_6[x_hw];
x_gr ^= x_hx;
x_hy = x_hc >> 0ULL;
x_hz = x_hy & 0x3fULL;
x_i0 = s_box_7[x_hz];
x_gr ^= x_i0;
x_i1 = x_ff ^ x_gr;
x_i2 = 0;
x_i3 = x_i1 & 281474976710655ULL;
x_i4 = x_i3 << 47ULL;
x_i5 = x_i3 << 15ULL;
x_i6 = x_i5 & 0x7c0000000000ULL;
x_i7 = x_i3 << 13ULL;
x_i8 = x_i7 & 0x3f000000000ULL;
x_i9 = x_i3 << 11ULL;
x_ia = x_i9 & 0xfc0000000ULL;
x_ib = x_i3 << 9ULL;
x_ic = x_ib & 0x3f000000ULL;
x_id = x_i3 << 7ULL;
x_ie = x_id & 0xfc0000ULL;
x_if = x_i3 << 5ULL;
x_ig = x_if & 0x3f000ULL;
x_ih = x_i3 << 3ULL;
x_ii = x_ih & 0xfc0ULL;
x_ij = x_i3 << 1ULL;
x_ik = x_ij & 0x3eULL;
x_il = x_i3 >> 31ULL;
x_im = x_i4;
x_im |= x_i6;
x_im |= x_i8;
x_im |= x_ia;
x_im |= x_ic;
x_im |= x_ie;
x_im |= x_ig;
x_im |= x_ii;
x_im |= x_ik;
x_im |= x_il;
x_in = x_im ^ key_11;
x_io = x_in >> 42ULL;
x_ip = x_io & 0x3fULL;
x_iq = s_box_0[x_ip];
x_i2 ^= x_iq;
x_ir = x_in >> 36ULL;
x_is = x_ir & 0x3fULL;
x_it = s_box_1[x_is];
x_i2 ^= x_it;
x_iu = x_in >> 30ULL;
x_iv = x_iu & 0x3fULL;
x_iw = s_box_2[x_iv];
x_i2 ^= x_iw;
x_ix = x_in >> 24ULL;
x_iy = x_ix & 0x3fULL;
x_iz = s_box_3[x_iy];
x_i2 ^= x_iz;
x_j0 = x_in >> 18ULL;
x_j1 = x_j0 & 0x3fULL;
x_j2 = s_box_4[x_j1];
x_i2 ^= x_j2;
x_j3 = x_in >> 12ULL;
x_j4 = x_j3 & 0x3fULL;
x_j5 = s_box_5[x_j4];
x_i2 ^= x_j5;
x_j6 = x_in >> 6ULL;
x_j7 = x_j6 & 0x3fULL;
x_j8 = s_box_6[x_j7];
x_i2 ^= x_j8;
x_j9 = x_in >> 0ULL;
x_ja = x_j9 & 0x3fULL;
x_jb = s_box_7[x_ja];
x_i2 ^= x_jb;
x_jc = x_gq ^ x_i2;
x_jd = 0;
x_je = x_jc & 281474976710655ULL;
x_jf = x_je << 47ULL;
x_jg = x_je << 15ULL;
x_jh = x_jg & 0x7c0000000000ULL;
x_ji = x_je << 13ULL;
x_jj = x_ji & 0x3f000000000ULL;
x_jk = x_je << 11ULL;
x_jl = x_jk & 0xfc0000000ULL;
x_jm = x_je << 9ULL;
x_jn = x_jm & 0x3f000000ULL;
x_jo = x_je << 7ULL;
x_jp = x_jo & 0xfc0000ULL;
x_jq = x_je << 5ULL;
x_jr = x_jq & 0x3f000ULL;
x_js = x_je << 3ULL;
x_jt = x_js & 0xfc0ULL;
x_ju = x_je << 1ULL;
x_jv = x_ju & 0x3eULL;
x_jw = x_je >> 31ULL;
x_jx = x_jf;
x_jx |= x_jh;
x_jx |= x_jj;
x_jx |= x_jl;
x_jx |= x_jn;
x_jx |= x_jp;
x_jx |= x_jr;
x_jx |= x_jt;
x_jx |= x_jv;
x_jx |= x_jw;
x_jy = x_jx ^ key_12;
x_jz = x_jy >> 42ULL;
x_k0 = x_jz & 0x3fULL;
x_k1 = s_box_0[x_k0];
x_jd ^= x_k1;
x_k2 = x_jy >> 36ULL;
x_k3 = x_k2 & 0x3fULL;
x_k4 = s_box_1[x_k3];
x_jd ^= x_k4;
x_k5 = x_jy >> 30ULL;
x_k6 = x_k5 & 0x3fULL;
x_k7 = s_box_2[x_k6];
x_jd ^= x_k7;
x_k8 = x_jy >> 24ULL;
x_k9 = x_k8 & 0x3fULL;
x_ka = s_box_3[x_k9];
x_jd ^= x_ka;
x_kb = x_jy >> 18ULL;
x_kc = x_kb & 0x3fULL;
x_kd = s_box_4[x_kc];
x_jd ^= x_kd;
x_ke = x_jy >> 12ULL;
x_kf = x_ke & 0x3fULL;
x_kg = s_box_5[x_kf];
x_jd ^= x_kg;
x_kh = x_jy >> 6ULL;
x_ki = x_kh & 0x3fULL;
x_kj = s_box_6[x_ki];
x_jd ^= x_kj;
x_kk = x_jy >> 0ULL;
x_kl = x_kk & 0x3fULL;
x_km = s_box_7[x_kl];
x_jd ^= x_km;
x_kn = x_i1 ^ x_jd;
x_ko = 0;
x_kp = x_kn & 281474976710655ULL;
x_kq = x_kp << 47ULL;
x_kr = x_kp << 15ULL;
x_ks = x_kr & 0x7c0000000000ULL;
x_kt = x_kp << 13ULL;
x_ku = x_kt & 0x3f000000000ULL;
x_kv = x_kp << 11ULL;
x_kw = x_kv & 0xfc0000000ULL;
x_kx = x_kp << 9ULL;
x_ky = x_kx & 0x3f000000ULL;
x_kz = x_kp << 7ULL;
x_l0 = x_kz & 0xfc0000ULL;
x_l1 = x_kp << 5ULL;
x_l2 = x_l1 & 0x3f000ULL;
x_l3 = x_kp << 3ULL;
x_l4 = x_l3 & 0xfc0ULL;
x_l5 = x_kp << 1ULL;
x_l6 = x_l5 & 0x3eULL;
x_l7 = x_kp >> 31ULL;
x_l8 = x_kq;
x_l8 |= x_ks;
x_l8 |= x_ku;
x_l8 |= x_kw;
x_l8 |= x_ky;
x_l8 |= x_l0;
x_l8 |= x_l2;
x_l8 |= x_l4;
x_l8 |= x_l6;
x_l8 |= x_l7;
x_l9 = x_l8 ^ key_13;
x_la = x_l9 >> 42ULL;
x_lb = x_la & 0x3fULL;
x_lc = s_box_0[x_lb];
x_ko ^= x_lc;
x_ld = x_l9 >> 36ULL;
x_le = x_ld & 0x3fULL;
x_lf = s_box_1[x_le];
x_ko ^= x_lf;
x_lg = x_l9 >> 30ULL;
x_lh = x_lg & 0x3fULL;
x_li = s_box_2[x_lh];
x_ko ^= x_li;
x_lj = x_l9 >> 24ULL;
x_lk = x_lj & 0x3fULL;
x_ll = s_box_3[x_lk];
x_ko ^= x_ll;
x_lm = x_l9 >> 18ULL;
x_ln = x_lm & 0x3fULL;
x_lo = s_box_4[x_ln];
x_ko ^= x_lo;
x_lp = x_l9 >> 12ULL;
x_lq = x_lp & 0x3fULL;
x_lr = s_box_5[x_lq];
x_ko ^= x_lr;
x_ls = x_l9 >> 6ULL;
x_lt = x_ls & 0x3fULL;
x_lu = s_box_6[x_lt];
x_ko ^= x_lu;
x_lv = x_l9 >> 0ULL;
x_lw = x_lv & 0x3fULL;
x_lx = s_box_7[x_lw];
x_ko ^= x_lx;
x_ly = x_jc ^ x_ko;
x_lz = 0;
x_m0 = x_ly & 281474976710655ULL;
x_m1 = x_m0 << 47ULL;
x_m2 = x_m0 << 15ULL;
x_m3 = x_m2 & 0x7c0000000000ULL;
x_m4 = x_m0 << 13ULL;
x_m5 = x_m4 & 0x3f000000000ULL;
x_m6 = x_m0 << 11ULL;
x_m7 = x_m6 & 0xfc0000000ULL;
x_m8 = x_m0 << 9ULL;
x_m9 = x_m8 & 0x3f000000ULL;
x_ma = x_m0 << 7ULL;
x_mb = x_ma & 0xfc0000ULL;
x_mc = x_m0 << 5ULL;
x_md = x_mc & 0x3f000ULL;
x_me = x_m0 << 3ULL;
x_mf = x_me & 0xfc0ULL;
x_mg = x_m0 << 1ULL;
x_mh = x_mg & 0x3eULL;
x_mi = x_m0 >> 31ULL;
x_mj = x_m1;
x_mj |= x_m3;
x_mj |= x_m5;
x_mj |= x_m7;
x_mj |= x_m9;
x_mj |= x_mb;
x_mj |= x_md;
x_mj |= x_mf;
x_mj |= x_mh;
x_mj |= x_mi;
x_mk = x_mj ^ key_14;
x_ml = x_mk >> 42ULL;
x_mm = x_ml & 0x3fULL;
x_mn = s_box_0[x_mm];
x_lz ^= x_mn;
x_mo = x_mk >> 36ULL;
x_mp = x_mo & 0x3fULL;
x_mq = s_box_1[x_mp];
x_lz ^= x_mq;
x_mr = x_mk >> 30ULL;
x_ms = x_mr & 0x3fULL;
x_mt = s_box_2[x_ms];
x_lz ^= x_mt;
x_mu = x_mk >> 24ULL;
x_mv = x_mu & 0x3fULL;
x_mw = s_box_3[x_mv];
x_lz ^= x_mw;
x_mx = x_mk >> 18ULL;
x_my = x_mx & 0x3fULL;
x_mz = s_box_4[x_my];
x_lz ^= x_mz;
x_n0 = x_mk >> 12ULL;
x_n1 = x_n0 & 0x3fULL;
x_n2 = s_box_5[x_n1];
x_lz ^= x_n2;
x_n3 = x_mk >> 6ULL;
x_n4 = x_n3 & 0x3fULL;
x_n5 = s_box_6[x_n4];
x_lz ^= x_n5;
x_n6 = x_mk >> 0ULL;
x_n7 = x_n6 & 0x3fULL;
x_n8 = s_box_7[x_n7];
x_lz ^= x_n8;
x_n9 = x_kn ^ x_lz;
x_na = 0;
x_nb = x_n9 & 281474976710655ULL;
x_nc = x_nb << 47ULL;
x_nd = x_nb << 15ULL;
x_ne = x_nd & 0x7c0000000000ULL;
x_nf = x_nb << 13ULL;
x_ng = x_nf & 0x3f000000000ULL;
x_nh = x_nb << 11ULL;
x_ni = x_nh & 0xfc0000000ULL;
x_nj = x_nb << 9ULL;
x_nk = x_nj & 0x3f000000ULL;
x_nl = x_nb << 7ULL;
x_nm = x_nl & 0xfc0000ULL;
x_nn = x_nb << 5ULL;
x_no = x_nn & 0x3f000ULL;
x_np = x_nb << 3ULL;
x_nq = x_np & 0xfc0ULL;
x_nr = x_nb << 1ULL;
x_ns = x_nr & 0x3eULL;
x_nt = x_nb >> 31ULL;
x_nu = x_nc;
x_nu |= x_ne;
x_nu |= x_ng;
x_nu |= x_ni;
x_nu |= x_nk;
x_nu |= x_nm;
x_nu |= x_no;
x_nu |= x_nq;
x_nu |= x_ns;
x_nu |= x_nt;
x_nv = x_nu ^ key_15;
x_nw = x_nv >> 42ULL;
x_nx = x_nw & 0x3fULL;
x_ny = s_box_0[x_nx];
x_na ^= x_ny;
x_nz = x_nv >> 36ULL;
x_o0 = x_nz & 0x3fULL;
x_o1 = s_box_1[x_o0];
x_na ^= x_o1;
x_o2 = x_nv >> 30ULL;
x_o3 = x_o2 & 0x3fULL;
x_o4 = s_box_2[x_o3];
x_na ^= x_o4;
x_o5 = x_nv >> 24ULL;
x_o6 = x_o5 & 0x3fULL;
x_o7 = s_box_3[x_o6];
x_na ^= x_o7;
x_o8 = x_nv >> 18ULL;
x_o9 = x_o8 & 0x3fULL;
x_oa = s_box_4[x_o9];
x_na ^= x_oa;
x_ob = x_nv >> 12ULL;
x_oc = x_ob & 0x3fULL;
x_od = s_box_5[x_oc];
x_na ^= x_od;
x_oe = x_nv >> 6ULL;
x_of = x_oe & 0x3fULL;
x_og = s_box_6[x_of];
x_na ^= x_og;
x_oh = x_nv >> 0ULL;
x_oi = x_oh & 0x3fULL;
x_oj = s_box_7[x_oi];
x_na ^= x_oj;
x_ok = x_ly ^ x_na;
x_ol = x_ok & 18446744073709551615ULL;
x_om = x_ol << 32ULL;
x_on = x_n9 & 18446744073709551615ULL;
x_oo = x_om;
x_oo |= x_on;
x_op = 0;
x_oq = x_oo << 39ULL;
x_or = x_oq & 0x8000000000000000ULL;
x_op |= x_or;
x_os = x_oo << 6ULL;
x_ot = x_os & 0x4000000000000000ULL;
x_op |= x_ot;
x_ou = x_oo << 45ULL;
x_ov = x_ou & 0x2000000000000000ULL;
x_op |= x_ov;
x_ow = x_oo << 12ULL;
x_ox = x_ow & 0x1000000000000000ULL;
x_op |= x_ox;
x_oy = x_oo << 51ULL;
x_oz = x_oy & 0x800000000000000ULL;
x_op |= x_oz;
x_p0 = x_oo << 18ULL;
x_p1 = x_p0 & 0x400000000000000ULL;
x_op |= x_p1;
x_p2 = x_oo << 57ULL;
x_p3 = x_p2 & 0x200000000000000ULL;
x_op |= x_p3;
x_p4 = x_oo << 24ULL;
x_p5 = x_p4 & 0x100000000000000ULL;
x_op |= x_p5;
x_p6 = x_oo << 30ULL;
x_p7 = x_p6 & 0x80000000000000ULL;
x_op |= x_p7;
x_p8 = x_oo >> 3ULL;
x_p9 = x_p8 & 0x40000000000000ULL;
x_op |= x_p9;
x_pa = x_oo << 36ULL;
x_pb = x_pa & 0x20000000000000ULL;
x_op |= x_pb;
x_pc = x_oo << 3ULL;
x_pd = x_pc & 0x10000000000000ULL;
x_op |= x_pd;
x_pe = x_oo << 42ULL;
x_pf = x_pe & 0x8000000000000ULL;
x_op |= x_pf;
x_pg = x_oo << 9ULL;
x_ph = x_pg & 0x4000000000000ULL;
x_op |= x_ph;
x_pi = x_oo << 48ULL;
x_pj = x_pi & 0x2000000000000ULL;
x_op |= x_pj;
x_pk = x_oo << 15ULL;
x_pl = x_pk & 0x1000000000000ULL;
x_op |= x_pl;
x_pm = x_oo << 21ULL;
x_pn = x_pm & 0x800000000000ULL;
x_op |= x_pn;
x_po = x_oo >> 12ULL;
x_pp = x_po & 0x400000000000ULL;
x_op |= x_pp;
x_pq = x_oo << 27ULL;
x_pr = x_pq & 0x200000000000ULL;
x_op |= x_pr;
x_ps = x_oo >> 6ULL;
x_pt = x_ps & 0x100000000000ULL;
x_op |= x_pt;
x_pu = x_oo << 33ULL;
x_pv = x_pu & 0x80000000000ULL;
x_op |= x_pv;
x_pw = x_oo << 0ULL;
x_px = x_pw & 0x40000000000ULL;
x_op |= x_px;
x_py = x_oo << 39ULL;
x_pz = x_py & 0x20000000000ULL;
x_op |= x_pz;
x_q0 = x_oo << 6ULL;
x_q1 = x_q0 & 0x10000000000ULL;
x_op |= x_q1;
x_q2 = x_oo << 12ULL;
x_q3 = x_q2 & 0x8000000000ULL;
x_op |= x_q3;
x_q4 = x_oo >> 21ULL;
x_q5 = x_q4 & 0x4000000000ULL;
x_op |= x_q5;
x_q6 = x_oo << 18ULL;
x_q7 = x_q6 & 0x2000000000ULL;
x_op |= x_q7;
x_q8 = x_oo >> 15ULL;
x_q9 = x_q8 & 0x1000000000ULL;
x_op |= x_q9;
x_qa = x_oo << 24ULL;
x_qb = x_qa & 0x800000000ULL;
x_op |= x_qb;
x_qc = x_oo >> 9ULL;
x_qd = x_qc & 0x400000000ULL;
x_op |= x_qd;
x_qe = x_oo << 30ULL;
x_qf = x_qe & 0x200000000ULL;
x_op |= x_qf;
x_qg = x_oo >> 3ULL;
x_qh = x_qg & 0x100000000ULL;
x_op |= x_qh;
x_qi = x_oo << 3ULL;
x_qj = x_qi & 0x80000000ULL;
x_op |= x_qj;
x_qk = x_oo >> 30ULL;
x_ql = x_qk & 0x40000000ULL;
x_op |= x_ql;
x_qm = x_oo << 9ULL;
x_qn = x_qm & 0x20000000ULL;
x_op |= x_qn;
x_qo = x_oo >> 24ULL;
x_qp = x_qo & 0x10000000ULL;
x_op |= x_qp;
x_qq = x_oo << 15ULL;
x_qr = x_qq & 0x8000000ULL;
x_op |= x_qr;
x_qs = x_oo >> 18ULL;
x_qt = x_qs & 0x4000000ULL;
x_op |= x_qt;
x_qu = x_oo << 21ULL;
x_qv = x_qu & 0x2000000ULL;
x_op |= x_qv;
x_qw = x_oo >> 12ULL;
x_qx = x_qw & 0x1000000ULL;
x_op |= x_qx;
x_qy = x_oo >> 6ULL;
x_qz = x_qy & 0x800000ULL;
x_op |= x_qz;
x_r0 = x_oo >> 39ULL;
x_r1 = x_r0 & 0x400000ULL;
x_op |= x_r1;
x_r2 = x_oo << 0ULL;
x_r3 = x_r2 & 0x200000ULL;
x_op |= x_r3;
x_r4 = x_oo >> 33ULL;
x_r5 = x_r4 & 0x100000ULL;
x_op |= x_r5;
x_r6 = x_oo << 6ULL;
x_r7 = x_r6 & 0x80000ULL;
x_op |= x_r7;
x_r8 = x_oo >> 27ULL;
x_r9 = x_r8 & 0x40000ULL;
x_op |= x_r9;
x_ra = x_oo << 12ULL;
x_rb = x_ra & 0x20000ULL;
x_op |= x_rb;
x_rc = x_oo >> 21ULL;
x_rd = x_rc & 0x10000ULL;
x_op |= x_rd;
x_re = x_oo >> 15ULL;
x_rf = x_re & 0x8000ULL;
x_op |= x_rf;
x_rg = x_oo >> 48ULL;
x_rh = x_rg & 0x4000ULL;
x_op |= x_rh;
x_ri = x_oo >> 9ULL;
x_rj = x_ri & 0x2000ULL;
x_op |= x_rj;
x_rk = x_oo >> 42ULL;
x_rl = x_rk & 0x1000ULL;
x_op |= x_rl;
x_rm = x_oo >> 3ULL;
x_rn = x_rm & 0x800ULL;
x_op |= x_rn;
x_ro = x_oo >> 36ULL;
x_rp = x_ro & 0x400ULL;
x_op |= x_rp;
x_rq = x_oo << 3ULL;
x_rr = x_rq & 0x200ULL;
x_op |= x_rr;
x_rs = x_oo >> 30ULL;
x_rt = x_rs & 0x100ULL;
x_op |= x_rt;
x_ru = x_oo >> 24ULL;
x_rv = x_ru & 0x80ULL;
x_op |= x_rv;
x_rw = x_oo >> 57ULL;
x_rx = x_rw & 0x40ULL;
x_op |= x_rx;
x_ry = x_oo >> 18ULL;
x_rz = x_ry & 0x20ULL;
x_op |= x_rz;
x_s0 = x_oo >> 51ULL;
x_s1 = x_s0 & 0x10ULL;
x_op |= x_s1;
x_s2 = x_oo >> 12ULL;
x_s3 = x_s2 & 0x8ULL;
x_op |= x_s3;
x_s4 = x_oo >> 45ULL;
x_s5 = x_s4 & 0x4ULL;
x_op |= x_s5;
x_s6 = x_oo >> 6ULL;
x_s7 = x_s6 & 0x2ULL;
x_op |= x_s7;
x_s8 = x_oo >> 39ULL;
x_s9 = x_s8 & 0x1ULL;
x_op |= x_s9;
return x_op;
}
