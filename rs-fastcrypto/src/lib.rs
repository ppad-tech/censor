// FFI bridge exposing fastcrypto::secp256r1 signing for the
// censor-demo-ffi harness. The defect of interest is the
// table-indexed scalar multiplication inside
// fastcrypto::groups::multiplier::windowed::WindowedScalarMultiplier::mul,
// reached on every sign through
//
//   Secp256r1KeyPair::sign
//     -> sign_with_hash::<DefaultHash>
//     -> sign_common::<H>
//     -> MULTIPLIER.mul(&secp256r1::Scalar(k))
//
// Class A holds the secret scalar fixed across pairs; class B draws
// fresh secrets. If the windowed multiplier's table accesses depend
// on the scalar's bits (they do), per-call timing diverges between
// the two classes and the e-process will reject decisively.

use fastcrypto::secp256r1::Secp256r1KeyPair;
use fastcrypto::traits::{Signer, ToFromBytes};
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
