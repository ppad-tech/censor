// FFI targets exercised by censor-demo-ffi.
//
// Each function takes two raw 32-byte pointers and returns a u8
// inequality witness: 0 iff the two inputs are byte-identical,
// nonzero otherwise. The control-flow shape of the returned value
// is the same across both functions; only the timing of the work
// itself differs.
//
// Layout reminder for the Haskell side:
//   workspace = [a (32B) | b (32B)]   (64 bytes total)
//
// std is linked in; the per-call code path doesn't touch it.

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
