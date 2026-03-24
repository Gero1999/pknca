# PKNCA Potential Bugs

This document records potential bugs, logic issues, and edge cases identified during the code audit. Each entry has been reviewed against the surrounding code context. Entries are classified as:

- **Confirmed concern** — the code path could produce incorrect results under described conditions
- **Likely safe** — the issue appears handled elsewhere but deserves a comment or test
- **False positive** — the initial concern was found to be handled correctly upon closer inspection

---

## 1. ~~Log Integration Division by Zero When conc.1 == conc.2~~ (Resolved)

**File:** `R/auc_integrate.R`
**Resolution:** Not a bug. `choose_interval_method()` only assigns `"log"` to strictly declining intervals where neither endpoint is zero, so `conc.1 == conc.2` cannot reach `aucintegrate_log()` or `aumcintegrate_log()`. A comment has been added to both functions documenting this precondition.

---

## 2. ~~Log of Zero in `interpolate_conc_log()`~~ (Resolved)

**File:** `R/auc_integrate.R`
**Resolution:** Not a bug. `choose_interval_method()` assigns `"zero"` or `"linear"` to any interval where either endpoint is zero, so `interpolate_conc_log()` will never receive a zero or negative concentration. A comment has been added to the function documenting this precondition.

---

## 3. ~~Floating Point Equality in `which(time == tlast)`~~ (Resolved)

**File:** `R/auc_integrate.R`
**Resolution:** An explicit check on `length(idx_tlast) == 1` was added after the `which(time == tlast)` call (but after the all-zeros early return, since `tlast` is `NA` for all-zero data). An informative error is now raised if `tlast` is not found or is found more than once. A test using the classic `0.1 + 0.2 != 0.3` floating point case was added to `test-auc_integrate.R` to verify the error fires correctly.

---

## 4. ~~Half-Life Selection Warning as Side Effect in Mask Construction~~ (Resolved)

**File:** `R/half.life.R`
**Resolution:** Not a bug. Immediately after the `mask_best` construction there is already a `sum(mask_best) > 1` tie-breaking block that selects the fit with the most points. When the `min.hl.points == 2` edge case fires, selecting all positive-lambda.z fits via `TRUE` and then resolving via the tie-breaker is the intended behavior. A comment has been added to the `mask_best` construction explaining this.

---

## 5. ~~All-BLQ Sentinel Value `tlast = tfirst + 1` in `clean.conc.blq()`~~ (Resolved)

**File:** `R/cleaners.R`
**Resolution:** Not a bug. The sentinel value `tlast = tfirst + 1` is only ever compared to `ret$time` in mask operations (e.g., `tlast <= ret$time`). Since `tfirst = max(ret$time)`, the sentinel is guaranteed to exceed all observed time values, correctly making the "last" mask empty for all-BLQ data. A comment has been added explaining this intent.

---

## 6. ~~`|` Instead of `||` for Scalar Comparisons~~ (Resolved)

**Resolution:** All scalar `if()` conditions using `|` and `&` have been updated to `||` and `&&` throughout the R source files. Vectorized uses of `|` and `&` (inside `all()`, `any()`, or column-level mask construction) were left unchanged. All 2292 tests pass.

---

## 7. ~~`conc %in% 0` Pattern~~ (Resolved)

**Resolution:** Not a bug. BLQ concentrations are cleaned to exactly 0 by `clean.conc.blq()` before any AUC or interpolation logic runs. Exact equality via `%in% 0` is therefore definitionally correct; a tolerance cannot be used because what constitutes a "low" concentration is context-dependent. Comments have been added in `R/auc_integrate.R` and `R/cleaners.R` documenting this intent.

---

## 8. ~~Duplicate Time Points in `interpolate.conc()`~~ (Resolved)

**File:** `R/interpolate.conc.R`, approximately line 198
**Resolution:** Not a bug. `data$time` can never contain duplicate values because `assert_conc_time()` enforces uniqueness of time points before any interpolation logic runs. Duplicate times are therefore structurally impossible at this call site.

---

## 9. ~~`stopifnot(!duplicated(...))` Behavior~~ (Resolved)

**File:** `R/unit-support.R`, line 131
**Resolution:** Not a bug. `stopifnot()` accepts a vector and stops if **any** element is `FALSE`, so `stopifnot(!duplicated(x))` correctly stops if any element of `x` is duplicated. This is idiomatic and intentional R behavior; wrapping in `any()` would be redundant.

---

## 10. ~~`pk.calc.aucinf.obs` When `lambda.z` Is NA~~ (Resolved)

**File:** `R/auc.R`
**Resolution:** Intended behavior. When `lambda.z = NA`, `assert_lambdaz()` allows the missing value (`any.missing = TRUE`), and `aucintegrate_inf()` propagates the NA through `clast / lambda.z`, returning `NA` for the extrapolated portion and thus `NA` for the overall AUCinf. A test covering `pk.calc.auc.inf.obs(..., lambda.z = NA)` has been added to `tests/testthat/test-auc.R` confirming this returns `NA_real_`.

---

## 11. ~~`getGroups.PKNCAconc()` Formula Default Argument~~ (Resolved)

**File:** `R/class-PKNCAconc.R`, line 240
**Resolution:** Not a bug. `PKNCAconc` objects are immutable in normal use, so the lazy default `form = stats::formula(object)` will always evaluate to the same value. No change needed.

---

## Summary by Severity

| # | File | Severity | Classification |
|---|------|----------|----------------|
| 1 | `auc_integrate.R` log division by zero | — | ~~Resolved~~ — comment added to source |
| 2 | `auc_integrate.R` log of zero | — | ~~Resolved~~ — comment added to source |
| 3 | `auc_integrate.R` floating point `==` | — | ~~Resolved~~ — check + test added |
| 4 | `half.life.R` warning side effect in mask | — | ~~Resolved~~ — false positive; comment added |
| 5 | `cleaners.R` all-BLQ sentinel value | — | ~~Resolved~~ — false positive; comment added |
| 6 | `pk.calc.simple.R` `|` vs `||` | — | ~~Resolved~~ — `||`/`&&` applied throughout |
| 7 | `auc_integrate.R` `%in% 0` pattern | — | ~~Resolved~~ — exact zero is correct; comment added |
| 8 | `interpolate.conc.R` duplicate times | — | ~~Resolved~~ — false positive; `assert_conc_time()` prevents duplicates |
| 9 | `unit-support.R` `stopifnot` vector | — | ~~Resolved~~ — false positive; idiomatic and intentional |
| 10 | `pk.calc.simple.R` lambda.z = NA | — | ~~Resolved~~ — intended; test added confirming NA return |
| 11 | `class-PKNCAconc.R` lazy default | — | ~~Resolved~~ — false positive; PKNCAconc objects are immutable |
