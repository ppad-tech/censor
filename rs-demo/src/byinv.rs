// Bernstein-Yang modular inverter, lifted from halo2curves'
// `src/ff_ext/inverse.rs` (ctbyinv worktree state) with one
// deliberate edit: the `core::hint::black_box(mask)` wrappers
// have been *removed* from `cond_add` and `cond_neg`.
//
// In that configuration, the source still has constant-time
// semantics, but on x86-64 LLVM observes that `mask` is always
// 0 or u64::MAX and collapses the `& mask` clauses into a
// secret-dependent branch (the famous `jbe LBB1_5` site
// surfaced by substratum). On aarch64 the same source compiles
// to branchless `csel`-style instructions, so the leak does
// not materialise.
//
// The intent here is to give censor a target that is genuinely
// vulnerable on one architecture and genuinely constant-time
// on another, with the same compiled Rust source. See
// `ppad.tech/articles/introducing-substratum.md` for the
// underlying analysis.
//
// A single fixed-modulus inverter is instantiated for the
// bn254 scalar field Fr (254-bit prime, 4x u64 little-endian)
// with `adjuster = 1` -- we only care about the timing of the
// invert call path, not the value it returns.

use core::{
    cmp::PartialEq,
    ops::{Add, Mul, Neg, Sub},
};

use subtle::{Choice, ConstantTimeEq};

#[derive(Clone)]
struct CInt<const B: usize, const L: usize>(pub [u64; L]);

impl<const B: usize, const L: usize> CInt<B, L> {
    pub const MASK: u64 = u64::MAX >> (64 - B);
    pub const MINUS_ONE: Self = Self([Self::MASK; L]);
    pub const ZERO: Self = Self([0; L]);
    pub const ONE: Self = {
        let mut data = [0; L];
        data[0] = 1;
        Self(data)
    };

    pub fn shift(&self) -> Self {
        let mut data = [0; L];
        let sign_mask = (-(self.is_negative() as i64)) as u64 & Self::MASK;
        data[L - 1] = sign_mask;
        data[..L - 1].copy_from_slice(&self.0[1..]);
        Self(data)
    }

    pub fn lowest(&self) -> u64 {
        self.0[0]
    }

    pub fn is_negative(&self) -> bool {
        self.0[L - 1] > (Self::MASK >> 1)
    }

    pub fn ct_eq_cint(&self, other: &Self) -> Choice {
        let mut acc = Choice::from(1u8);
        let mut i = 0;
        while i < L {
            acc &= self.0[i].ct_eq(&other.0[i]);
            i += 1;
        }
        acc
    }

    // NOTE: the `let mask = core::hint::black_box(mask);` line
    // from the upstream version has been deliberately removed
    // here -- this is the whole point of this vendored copy.
    fn cond_add(&self, other: &Self, mask: u64) -> Self {
        let (mut data, mut carry) = ([0u64; L], 0u64);
        for (i, d) in data.iter_mut().enumerate().take(L) {
            let sum = self.0[i] + (other.0[i] & mask) + carry;
            *d = sum & Self::MASK;
            carry = sum >> B;
        }
        Self(data)
    }

    // NOTE: black_box removed, as with `cond_add`.
    fn cond_neg(&self, mask: u64) -> Self {
        let mask_b = mask & Self::MASK;
        let init_carry = mask & 1;
        let (mut data, mut carry) = ([0u64; L], init_carry);
        for (i, d) in data.iter_mut().enumerate().take(L) {
            let sum = (self.0[i] ^ mask_b) + carry;
            *d = sum & Self::MASK;
            carry = sum >> B;
        }
        Self(data)
    }
}

impl<const B: usize, const L: usize> PartialEq for CInt<B, L> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<const B: usize, const L: usize> Add for &CInt<B, L> {
    type Output = CInt<B, L>;
    fn add(self, other: Self) -> Self::Output {
        let (mut data, mut carry) = ([0; L], 0);
        for (i, d) in data.iter_mut().enumerate().take(L) {
            let sum = self.0[i] + other.0[i] + carry;
            *d = sum & CInt::<B, L>::MASK;
            carry = sum >> B;
        }
        CInt::<B, L>(data)
    }
}

impl<const B: usize, const L: usize> Add<&CInt<B, L>> for CInt<B, L> {
    type Output = CInt<B, L>;
    fn add(self, other: &Self) -> Self::Output {
        &self + other
    }
}

impl<const B: usize, const L: usize> Add for CInt<B, L> {
    type Output = CInt<B, L>;
    fn add(self, other: Self) -> Self::Output {
        &self + &other
    }
}

impl<const B: usize, const L: usize> Sub for &CInt<B, L> {
    type Output = CInt<B, L>;
    fn sub(self, other: Self) -> Self::Output {
        let (mut data, mut carry) = ([0; L], 1);
        for (i, d) in data.iter_mut().enumerate().take(L) {
            let sum = self.0[i] + (other.0[i] ^ CInt::<B, L>::MASK) + carry;
            *d = sum & CInt::<B, L>::MASK;
            carry = sum >> B;
        }
        CInt::<B, L>(data)
    }
}

impl<const B: usize, const L: usize> Sub<&CInt<B, L>> for CInt<B, L> {
    type Output = CInt<B, L>;
    fn sub(self, other: &Self) -> Self::Output {
        &self - other
    }
}

impl<const B: usize, const L: usize> Sub for CInt<B, L> {
    type Output = CInt<B, L>;
    fn sub(self, other: Self) -> Self::Output {
        &self - &other
    }
}

impl<const B: usize, const L: usize> Neg for &CInt<B, L> {
    type Output = CInt<B, L>;
    fn neg(self) -> Self::Output {
        let (mut data, mut carry) = ([0; L], 1);
        for (i, d) in data.iter_mut().enumerate().take(L) {
            let sum = (self.0[i] ^ CInt::<B, L>::MASK) + carry;
            *d = sum & CInt::<B, L>::MASK;
            carry = sum >> B;
        }
        CInt::<B, L>(data)
    }
}

impl<const B: usize, const L: usize> Neg for CInt<B, L> {
    type Output = CInt<B, L>;
    fn neg(self) -> Self::Output {
        -&self
    }
}

impl<const B: usize, const L: usize> Mul for &CInt<B, L> {
    type Output = CInt<B, L>;
    fn mul(self, other: Self) -> Self::Output {
        let mut data = [0; L];
        for i in 0..L {
            let mut carry = 0;
            for k in 0..(L - i) {
                let sum = (data[i + k] as u128)
                    + (carry as u128)
                    + (self.0[i] as u128) * (other.0[k] as u128);
                data[i + k] = sum as u64 & CInt::<B, L>::MASK;
                carry = (sum >> B) as u64;
            }
        }
        CInt::<B, L>(data)
    }
}

impl<const B: usize, const L: usize> Mul<&CInt<B, L>> for CInt<B, L> {
    type Output = CInt<B, L>;
    fn mul(self, other: &Self) -> Self::Output {
        &self * other
    }
}

impl<const B: usize, const L: usize> Mul for CInt<B, L> {
    type Output = CInt<B, L>;
    fn mul(self, other: Self) -> Self::Output {
        &self * &other
    }
}

impl<const B: usize, const L: usize> Mul<i64> for &CInt<B, L> {
    type Output = CInt<B, L>;
    fn mul(self, other: i64) -> Self::Output {
        let s = ((other >> 63) as i64) as u64;
        let other_abs = (((other ^ (s as i64)).wrapping_sub(s as i64))) as u64;
        let mut carry = other_abs & s;
        let mask = s & CInt::<B, L>::MASK;
        let mut data = [0u64; L];
        for (i, d) in data.iter_mut().enumerate().take(L) {
            let sum = (carry as u128)
                + ((self.0[i] ^ mask) as u128) * (other_abs as u128);
            *d = sum as u64 & CInt::<B, L>::MASK;
            carry = (sum >> B) as u64;
        }
        CInt::<B, L>(data)
    }
}

impl<const B: usize, const L: usize> Mul<i64> for CInt<B, L> {
    type Output = CInt<B, L>;
    fn mul(self, other: i64) -> Self::Output {
        &self * other
    }
}

impl<const B: usize, const L: usize> Mul<&CInt<B, L>> for i64 {
    type Output = CInt<B, L>;
    fn mul(self, other: &CInt<B, L>) -> Self::Output {
        other * self
    }
}

impl<const B: usize, const L: usize> Mul<CInt<B, L>> for i64 {
    type Output = CInt<B, L>;
    fn mul(self, other: CInt<B, L>) -> Self::Output {
        other * self
    }
}

pub struct BYInverter<const L: usize> {
    modulus: CInt<62, L>,
    adjuster: CInt<62, L>,
    inverse: i64,
}

type Matrix = [[i64; 2]; 2];

const fn outer_iterations(l: usize) -> usize {
    let d = 62 * l - 64;
    let divsteps = (49 * d + 80 + 16) / 17;
    (divsteps + 61) / 62
}

impl<const L: usize> BYInverter<L> {
    fn jump(f0: &CInt<62, L>, g0: &CInt<62, L>, mut delta: i64) -> (i64, Matrix) {
        let mut f = f0.lowest() as i64;
        let mut g = g0.lowest() as i64;
        let (mut u, mut v, mut q, mut r): (i64, i64, i64, i64) = (1, 0, 0, 1);

        let mut i = 0;
        while i < 62 {
            let mask_delta_pos = !(delta.wrapping_sub(1) >> 63);
            let mask_g_odd = (g & 1).wrapping_neg();
            let swap = mask_delta_pos & mask_g_odd;

            let f_for_combo = (f ^ swap).wrapping_sub(swap);
            let new_f = f ^ ((f ^ g) & swap);
            let g_add = f_for_combo & mask_g_odd;
            let new_g = g.wrapping_add(g_add) >> 1;
            let new_delta = (delta ^ swap).wrapping_sub(swap).wrapping_add(1);

            let u_post = u ^ ((u ^ q) & swap);
            let v_post = v ^ ((v ^ r) & swap);
            let q_pre = q ^ ((q ^ u) & swap);
            let r_pre = r ^ ((r ^ v) & swap);
            let q_neg = (q_pre ^ swap).wrapping_sub(swap);
            let r_neg = (r_pre ^ swap).wrapping_sub(swap);
            let new_q = q_neg.wrapping_add(u_post & mask_g_odd);
            let new_r = r_neg.wrapping_add(v_post & mask_g_odd);
            let new_u = u_post.wrapping_shl(1);
            let new_v = v_post.wrapping_shl(1);

            f = new_f;
            g = new_g;
            delta = new_delta;
            u = new_u;
            v = new_v;
            q = new_q;
            r = new_r;

            i += 1;
        }

        (delta, [[u, v], [q, r]])
    }

    fn fg(f: CInt<62, L>, g: CInt<62, L>, t: Matrix) -> (CInt<62, L>, CInt<62, L>) {
        (
            (t[0][0] * &f + t[0][1] * &g).shift(),
            (t[1][0] * &f + t[1][1] * &g).shift(),
        )
    }

    fn de(&self, d: CInt<62, L>, e: CInt<62, L>, t: Matrix) -> (CInt<62, L>, CInt<62, L>) {
        let mask = CInt::<62, L>::MASK as i64;
        let mut md = t[0][0] * d.is_negative() as i64 + t[0][1] * e.is_negative() as i64;
        let mut me = t[1][0] * d.is_negative() as i64 + t[1][1] * e.is_negative() as i64;

        let cd = t[0][0]
            .wrapping_mul(d.lowest() as i64)
            .wrapping_add(t[0][1].wrapping_mul(e.lowest() as i64))
            & mask;
        let ce = t[1][0]
            .wrapping_mul(d.lowest() as i64)
            .wrapping_add(t[1][1].wrapping_mul(e.lowest() as i64))
            & mask;

        md -= (self.inverse.wrapping_mul(cd).wrapping_add(md)) & mask;
        me -= (self.inverse.wrapping_mul(ce).wrapping_add(me)) & mask;

        let cd = t[0][0] * &d + t[0][1] * &e + md * &self.modulus;
        let ce = t[1][0] * &d + t[1][1] * &e + me * &self.modulus;

        (cd.shift(), ce.shift())
    }

    fn norm(&self, value: CInt<62, L>, negate: Choice) -> CInt<62, L> {
        let neg1 = (-(value.is_negative() as i64)) as u64;
        let v1 = value.cond_add(&self.modulus, neg1);

        let neg_mask = (-(negate.unwrap_u8() as i64)) as u64;
        let v2 = v1.cond_neg(neg_mask);

        let neg2 = (-(v2.is_negative() as i64)) as u64;
        v2.cond_add(&self.modulus, neg2)
    }

    const fn convert<const I: usize, const O: usize, const S: usize>(input: &[u64]) -> [u64; S] {
        const fn min(a: usize, b: usize) -> usize {
            if a > b {
                b
            } else {
                a
            }
        }

        let (total, mut output, mut bits) = (min(input.len() * I, S * O), [0; S], 0);

        while bits < total {
            let (i, o) = (bits % I, bits % O);
            output[bits / O] |= (input[bits / I] >> i) << o;
            bits += min(I - i, O - o);
        }

        let mask = u64::MAX >> (64 - O);
        let mut filled = total / O + if total % O > 0 { 1 } else { 0 };

        while filled > 0 {
            filled -= 1;
            output[filled] &= mask;
        }

        output
    }

    const fn inv(value: u64) -> i64 {
        let x = value.wrapping_mul(3) ^ 2;
        let y = 1u64.wrapping_sub(x.wrapping_mul(value));
        let (x, y) = (x.wrapping_mul(y.wrapping_add(1)), y.wrapping_mul(y));
        let (x, y) = (x.wrapping_mul(y.wrapping_add(1)), y.wrapping_mul(y));
        let (x, y) = (x.wrapping_mul(y.wrapping_add(1)), y.wrapping_mul(y));
        (x.wrapping_mul(y.wrapping_add(1)) & CInt::<62, L>::MASK) as i64
    }

    pub const fn new(modulus: &[u64], adjuster: &[u64]) -> Self {
        Self {
            modulus: CInt::<62, L>(Self::convert::<64, 62, L>(modulus)),
            adjuster: CInt::<62, L>(Self::convert::<64, 62, L>(adjuster)),
            inverse: Self::inv(modulus[0]),
        }
    }

    pub fn invert<const S: usize>(&self, value: &[u64]) -> ([u64; S], Choice) {
        let (mut d, mut e) = (CInt::ZERO, self.adjuster.clone());
        let mut g = CInt::<62, L>(Self::convert::<64, 62, L>(value));
        let (mut delta, mut f) = (1i64, self.modulus.clone());

        let iters = outer_iterations(L);
        let mut k = 0;
        while k < iters {
            let (new_delta, matrix) = Self::jump(&f, &g, delta);
            delta = new_delta;
            let (nf, ng) = Self::fg(f, g, matrix);
            f = nf;
            g = ng;
            let (nd, ne) = self.de(d, e, matrix);
            d = nd;
            e = ne;
            k += 1;
        }

        let antiunit = f.ct_eq_cint(&CInt::MINUS_ONE);
        let is_one = f.ct_eq_cint(&CInt::ONE);
        let success = antiunit | is_one;

        let normed = self.norm(d, antiunit);
        let limbs = Self::convert::<62, 64, S>(&normed.0);
        (limbs, success)
    }
}

// bn254 scalar field Fr.
//
// modulus = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001
//   (~254 bits; little-endian 4x u64 below).
//
// adjuster = 1: the invert call returns the plain modular
// inverse rather than a Montgomery-form one. We don't use the
// result -- only the timing of the call -- so this keeps the
// vendored module self-contained without needing the R^2
// constant.
//
// L = 6: smallest value with 2^(62L - 64) > modulus. For a
// 254-bit modulus: 62*6 - 64 = 308 >= 254.

const FR_MODULUS: [u64; 4] = [
    0x43e1_f593_f000_0001,
    0x2833_e848_79b9_7091,
    0xb850_45b6_8181_585d,
    0x3064_4e72_e131_a029,
];

const FR_ADJUSTER: [u64; 4] = [1, 0, 0, 0];

const FR_L: usize = 6;

static FR_INVERTER: BYInverter<FR_L> =
    BYInverter::<FR_L>::new(&FR_MODULUS, &FR_ADJUSTER);

/// Invert a bn254 Fr scalar supplied as 32 little-endian bytes,
/// writing the result as 32 little-endian bytes. The returned
/// invertibility flag is discarded -- the timed call always
/// runs the full outer loop regardless of whether the input is
/// invertible.
pub fn invert_fr_le(input: &[u8; 32], output: &mut [u8; 32]) {
    let mut limbs = [0u64; 4];
    for (i, l) in limbs.iter_mut().enumerate() {
        let mut bytes = [0u8; 8];
        bytes.copy_from_slice(&input[i * 8..(i + 1) * 8]);
        *l = u64::from_le_bytes(bytes);
    }

    let (out, _success) = FR_INVERTER.invert::<4>(&limbs);

    for (i, l) in out.iter().enumerate() {
        let bytes = l.to_le_bytes();
        output[i * 8..(i + 1) * 8].copy_from_slice(&bytes);
    }
}
