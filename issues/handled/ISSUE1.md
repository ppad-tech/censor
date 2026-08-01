# ISSUE1 - demo-ffi does not compile against the
# current CaseReport; the flake's default package
# fails to build

Source: `synthesis` phase 3 (engagement compiler;
building a censor runner closure for the ATTACK164
dynamic battery from a pinned censor input).
Surfaced on the first `nix build` of
`censor.packages.<system>.default`.

## Summary

`CaseReport` has carried six fields since 3ffc043
(2026-07-18, `crBatch` / `crTrace`) and 977fa4d
(2026-07-25, `crNoise`). `demo-ffi/Main.hs:250`
still constructs it positionally with three:

    emit (R.CaseReport name r mat)

which GHC rejects:

    demo-ffi/Main.hs:250:9: error: [GHC-83865]
        • Couldn't match expected type 'R.CaseReport'
          with actual type
            'Maybe Int -> Maybe C.Noise
             -> Maybe [C.Frame] -> R.CaseReport'

Both commits updated the other constructor sites —
`demo/Main.hs:450` reads

    emit (R.CaseReport name r mat Nothing Nothing Nothing)

and `test/Main.hs` was likewise updated — but
`demo-ffi/Main.hs` was missed. It has not been
touched since d11e67c.

## Impact

`flake.nix` builds the demo executables by default:

    callCabal2nixWithOptions lib ./.
      "--flag=demo --flag=demo-ffi --flag=poly"

so `nix build` on master fails outright. Verified on
`aarch64-darwin`: the derivation compiles
`censor-aead`, `censor-demo`, and `censor-poly`
successfully and then dies on `censor-demo-ffi`.

A plain `cabal build` is unaffected, because
`demo-ffi` defaults to off. That is why the break
has survived roughly two weeks: the flag is off in
the path used interactively and on in the path used
by every downstream flake.

Downstream consequence: any flake consuming
`censor.packages.<system>.default` cannot build it.
`synthesis` currently works around this on its own
side, disabling the flag where it overrides censor's
package:

    disableCabalFlag (enableCabalFlag cen "aead")
      "demo-ffi"

That is a workaround for a build break, not a
preference, and should be removed once this is
fixed.

## Fix

The minimal repair is to pass the three optional
fields at the call site, matching `demo/Main.hs`:

    emit (R.CaseReport name r mat Nothing Nothing Nothing)

## Worth considering alongside it

Three positional `Nothing`s at every construction
site is what made this break easy to introduce and
easy to miss: adding an optional field to
`CaseReport` is currently a breaking change for
every constructor, and nothing distinguishes the
call sites that must be updated from the ones that
merely could be.

A smart constructor carrying the required fields,
with the optional ones defaulted and set by record
update where wanted, would make future field
additions non-breaking:

    caseReport :: String -> Result -> Maybe Attribution
               -> CaseReport

    (caseReport name r mat) { crBatch = Just b }

This is a suggestion, not part of the defect. The
build break should be fixed regardless.

## Prevention

The break is invisible to `cabal build` and to
`cabal test`, and visible only to a flake build. If
censor's checks do not already realise the flake's
default package, adding that would close the gap
that let a two-week-old compile error reach a
downstream consumer.

Adding the demo executables to `checks` — or simply
ensuring `nix flake check` builds
`packages.default` — would have caught 3ffc043 at
the commit that introduced it.

## Validation

  - `nix build` on the flake succeeds;
  - `censor-demo-ffi` runs its battery and emits a
    well-formed `censor/report-v3` document, with the
    three optional fields absent rather than
    misreported;
  - `cabal build -fdemo-ffi` succeeds against the
    rust staticlib;
  - if the smart constructor is adopted, existing
    JSON output is byte-identical for a case that
    sets none of the optional fields.
