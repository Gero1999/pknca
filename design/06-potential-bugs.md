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

## 6. `|` Instead of `||` for Scalar Comparisons

**File:** `R/pk.calc.simple.R`, line 33 (and several other locations)
**Classification:** Style issue, not a bug

```r
if (length(conc) == 0 | all(is.na(conc))) {
```

Both sides of `|` are scalar expressions here. Using `|` (element-wise OR) rather than `||` (short-circuit OR) means that `all(is.na(conc))` is always evaluated even when `length(conc) == 0`. For empty `conc`, `all(is.na(conc))` returns `TRUE` (vacuous truth), so this is correct, but using `||` would be slightly more efficient and more idiomatic for scalar conditions.

**Recommendation:** Replace `|` with `||` and `&` with `&&` in scalar `if()` conditions throughout the codebase. This is a low-risk style improvement.

---

## 7. `conc %in% 0` Pattern

**File:** `R/auc_integrate.R` line 103, `R/auc.R`, and others
**Classification:** Confirmed concern (subtle semantics)

```r
if (all(conc %in% 0)) {
```

The `%in%` operator uses `==` internally, which means it tests exact equality. For concentrations that are computationally zero (e.g., `1e-320` due to floating point underflow) this check would fail and the interval would not be treated as zero. Similarly, `%in%` is designed for set membership — using it to test equality to a scalar is non-idiomatic and could confuse readers.

**Recommendation:** Replace `conc %in% 0` with `conc == 0` (or `abs(conc) < .Machine$double.eps` if near-zero values are a concern). Add a comment if exact zero is definitionally correct here (i.e., BLQ values have been cleaned to exactly 0 upstream).

---

## 8. Duplicate Time Points in `interpolate.conc()`

**File:** `R/interpolate.conc.R`, approximately line 198
**Classification:** Likely safe, but edge case not clearly tested

```r
ret <- data$conc[time.out == data$time]
```

If `data$time` contains duplicate values (two measurements at the same time), this would return a vector of length > 1. The surrounding code expects a scalar. Duplicate times should be caught earlier in `assert_conc_time()`, but the behavior in this function if they are not caught is undefined.

**Recommendation:** Add a `stopifnot(length(ret) == 1)` guard after this assignment, or verify that the assertions in the call chain always prevent duplicate times from reaching this point and add a comment to that effect.

---

## 9. `stopifnot(!duplicated(...))` Behavior

**File:** `R/unit-support.R`, line 131
**Classification:** False positive — this is correct R behavior

```r
stopifnot(!duplicated(conversions$PPORRESU))
```

A review concern was raised that this might need `stopifnot(!any(duplicated(...)))`. However, `stopifnot()` accepts a vector and stops if **any** element is `FALSE`. So `stopifnot(!duplicated(x))` correctly stops if any element of `x` is duplicated. This is idiomatic and correct.

**No action needed.** A comment clarifying this behavior would be useful for future maintainers unfamiliar with `stopifnot()`'s vector semantics.

---

## 10. `pk.calc.aucinf.obs` When `lambda.z` Is NA

**File:** `R/pk.calc.simple.R`
**Classification:** Likely safe, deserves confirmation

If `lambda.z` is `NA` (no reliable terminal slope could be computed), `pk.calc.aucinf.obs()` should return `NA` rather than a calculated value. The `assert_lambdaz()` function checks that lambda.z is a positive number and allows missing values by default (`any.missing = TRUE`). The return behavior when `lambda.z = NA` should be tested explicitly.

**Recommendation:** Confirm there is a test covering `pk.calc.aucinf.obs(auc = 100, clast.obs = 1, lambda.z = NA)` and that it returns `NA` gracefully.

---

## 11. `getGroups.PKNCAconc()` Formula Default Argument

**File:** `R/class-PKNCAconc.R`, line 240
**Classification:** Likely safe, subtle behavior

```r
getGroups.PKNCAconc <- function(object, form=stats::formula(object), level,
                                data=as.data.frame(object), sep) {
```

The default value `form = stats::formula(object)` is evaluated lazily when the argument is first accessed. If `stats::formula.PKNCAconc()` depends on state that could be modified between function entry and first use of `form`, this could behave unexpectedly. In practice, `PKNCAconc` objects are immutable in normal use, so this is unlikely to manifest.

**No action needed.** Noted for awareness.

---

## Summary by Severity

| # | File | Severity | Classification |
|---|------|----------|----------------|
| 1 | `auc_integrate.R` log division by zero | — | ~~Resolved~~ — comment added to source |
| 2 | `auc_integrate.R` log of zero | — | ~~Resolved~~ — comment added to source |
| 3 | `auc_integrate.R` floating point `==` | — | ~~Resolved~~ — check + test added |
| 4 | `half.life.R` warning side effect in mask | — | ~~Resolved~~ — false positive; comment added |
| 5 | `cleaners.R` all-BLQ sentinel value | — | ~~Resolved~~ — false positive; comment added |
| 6 | `pk.calc.simple.R` `|` vs `||` | Low | Style issue |
| 7 | `auc_integrate.R` `%in% 0` pattern | Low | Confirmed concern — semantic clarity |
| 8 | `interpolate.conc.R` duplicate times | Low | Likely safe — needs guard/comment |
| 9 | `unit-support.R` `stopifnot` vector | — | False positive — code is correct |
| 10 | `pk.calc.simple.R` lambda.z = NA | Low | Needs test confirmation |
| 11 | `class-PKNCAconc.R` lazy default | Low | Likely safe |
