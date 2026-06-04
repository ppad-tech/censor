// FFI targets exercised by censor-demo-ffi. Two groups of
// functions:
//
//   * 32-byte inequality witnesses -- a controlled CT / leaky
//     pair, with workspace = [a (32B) | b (32B)].
//   * a fastcrypto::secp256r1 sign bridge -- exercises the
//     production sign path whose underlying scalar mul goes
//     through the table-indexed WindowedScalarMultiplier::mul.
//
// Both groups live in one crate so the Haskell side links a single
// staticlib and the Rust std runtime symbols (rust_eh_personality,
// ARGV_INIT_ARRAY, EMPTY_PANIC, ...) appear exactly once at link
// time -- ld.gold on Linux rejects duplicate definitions that
// Apple's ld silently accepts.
//
// std is linked in; the per-call code paths don't touch it.

/// Constant-time 32-byte inequality witness.
///
/// Walks all 32 bytes via an XOR fold; the loop count, branches,
/// and memory access pattern are independent of the byte values.
/// Returns 0 iff the inputs are byte-identical.
///
/// # Safety
///
/// `a` and `b` must each point to at least 32 bytes of readable
/// memory.
#[no_mangle]
pub unsafe extern "C" fn censor_ct_neq32(
    a: *const u8,
    b: *const u8,
) -> u8 {
    let mut acc: u8 = 0;
    for i in 0..32 {
        acc |= *a.add(i) ^ *b.add(i);
    }
    acc
}

/// Deliberately leaky 32-byte inequality witness.
///
/// Returns 0 iff the inputs are byte-identical, otherwise the
/// first differing byte's XOR. Short-circuits on the first
/// mismatch -- runtime is proportional to the position of the
/// first differing byte. Included as a negative control: a
/// CT-testing framework worth using must reject this.
///
/// # Safety
///
/// `a` and `b` must each point to at least 32 bytes of readable
/// memory.
#[no_mangle]
pub unsafe extern "C" fn censor_naive_neq32(
    a: *const u8,
    b: *const u8,
) -> u8 {
    for i in 0..32 {
        let d = *a.add(i) ^ *b.add(i);
        if d != 0 {
            return d;
        }
    }
    0
}

// fastcrypto secp256r1 sign bridge -------------------------------
//
// The defect of interest is the table-indexed scalar multiplication
// inside
//
//   fastcrypto::groups::multiplier::windowed::
//     WindowedScalarMultiplier::mul
//
// reached on every sign through
//
//   Secp256r1KeyPair::sign
//     -> sign_with_hash::<DefaultHash>
//     -> sign_common::<H>
//     -> MULTIPLIER.mul(&secp256r1::Scalar(k))
//
// Class A holds the secret scalar fixed across pairs; class B
// draws fresh secrets. If the windowed multiplier's table accesses
// depend on the scalar's bits (they do), per-call timing diverges
// between the two classes and the e-process will reject decisively.

use ::fastcrypto::secp256r1::Secp256r1KeyPair;
use ::fastcrypto::traits::{Signer, ToFromBytes};
use std::slice;

/// Build a Secp256r1KeyPair from a 32-byte private scalar. Caller
/// owns the returned pointer; release with
/// `h_secp256r1_keypair_free`. Returns null on failure (e.g. zero
/// or out-of-range scalar).
///
/// # Safety
///
/// `seed` must point to at least 32 readable bytes.
#[no_mangle]
pub unsafe extern "C" fn h_secp256r1_keypair_from_bytes(
    seed: *const u8,
) -> *mut Secp256r1KeyPair {
    let bytes = slice::from_raw_parts(seed, 32);
    match Secp256r1KeyPair::from_bytes(bytes) {
        Ok(kp) => Box::into_raw(Box::new(kp)),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Release a keypair allocated by `h_secp256r1_keypair_from_bytes`.
///
/// # Safety
///
/// `kp` must be a pointer previously returned by
/// `h_secp256r1_keypair_from_bytes`, and must not be used again
/// after this call.
#[no_mangle]
pub unsafe extern "C" fn h_secp256r1_keypair_free(
    kp: *mut Secp256r1KeyPair,
) {
    if !kp.is_null() {
        let _ = Box::from_raw(kp);
    }
}

/// Sign a message with the given keypair. This is the timed call:
/// it walks the leaky table-indexed scalar multiplication on every
/// invocation.
///
/// # Safety
///
/// `kp` must be a valid pointer from
/// `h_secp256r1_keypair_from_bytes`; `msg` must point to at least
/// `msg_len` readable bytes.
#[no_mangle]
pub unsafe extern "C" fn h_secp256r1_sign(
    kp: *const Secp256r1KeyPair,
    msg: *const u8,
    msg_len: usize,
) {
    let kp = &*kp;
    let m = slice::from_raw_parts(msg, msg_len);
    let _ = kp.sign(m);
}
