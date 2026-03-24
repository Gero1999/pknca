# PKNCA Potential Bugs

This document records potential bugs, logic issues, and edge cases identified during the code audit. Each entry has been reviewed against the surrounding code context. Entries are classified as:

- **Confirmed concern** — the code path could produce incorrect results under described conditions
- **Likely safe** — the issue appears handled elsewhere but deserves a comment or test
- **False positive** — the initial concern was found to be handled correctly upon closer inspection

---

## 1. Log Integration Division by Zero When conc.1 == conc.2

**File:** `R/auc_integrate.R`, lines 7–8, 19–21
**Classification:** Likely safe, but deserves a test

```r
aucintegrate_log <- function(conc.1, conc.2, time.1, time.2) {
  (time.2-time.1) * (conc.2-conc.1)/log(conc.2/conc.1)
}
aumcintegrate_log <- function(conc.1, conc.2, time.1, time.2) {
  ((time.2-time.1) * (conc.2*time.2-conc.1*time.1) / log(conc.2/conc.1)-
     (time.2-time.1)^2 * (conc.2-conc.1) / (log(conc.2/conc.1)^2))
}
```

When `conc.1 == conc.2`, `log(conc.2/conc.1) = log(1) = 0`, causing `0/0 = NaN` (since the numerator `conc.2 - conc.1` is also 0). Mathematically the limit of the log-trapezoidal formula when `conc.1 → conc.2` equals the linear trapezoidal formula result, but R will produce `NaN`.

**Mitigation:** `choose_interval_method()` in `auc_integrate.R` assigns `"log"` to intervals only when concentrations are declining (`conc[idx_2] < conc[idx_1]`) in `lin up/log down` mode, or post-tmax in `lin-log` mode, guarded by `conc[idx_2] != 0`. For truly equal concentrations going into `aucintegrate_log`, this would require `conc.1 == conc.2` with neither being zero, which in the `lin up/log down` method would be treated as "up" (linear). So this path should not be reached in normal use.

**Recommendation:** Add a guard or `stopifnot` to `aucintegrate_log` and `aumcintegrate_log` with a clear error message (e.g., `stopifnot(conc.1 != conc.2)`), or add a comment explaining why equal concentrations cannot reach this function. Add a test confirming the behavior when concentrations are equal.

---

## 2. Log of Zero in `interpolate_conc_log()`

**File:** `R/auc_integrate.R`, lines 46–51
**Classification:** Likely safe, but deserves a comment

```r
interpolate_conc_log <- function(conc_1, conc_2, time_1, time_2, time_out) {
  exp(
    log(conc_1) +
      (time_out-time_1)/(time_2-time_1)*(log(conc_2)-log(conc_1))
  )
}
```

If `conc_1 == 0` or `conc_2 == 0`, `log(0) = -Inf` and the result would be 0 or degenerate.

**Mitigation:** `choose_interval_method()` guards against zero concentrations at interval boundaries by assigning `"zero"` or `"linear"` methods when either endpoint is zero. The `interpolate_conc_log` function should therefore never receive a zero concentration.

**Recommendation:** Add a comment to `interpolate_conc_log` stating the precondition (`conc_1 > 0`, `conc_2 > 0`) and add a `stopifnot` in debug mode or an explicit check.

---

## 3. Floating Point Equality in `which(time == tlast)`

**File:** `R/auc_integrate.R`, line 96
**Classification:** Likely safe in practice, but theoretically fragile

```r
idx_tlast <- which(time == tlast)
```

If `tlast` is computed by `pk.calc.tlast()` which returns the actual value from the `time` vector, then `time == tlast` compares a value to itself — floating point equality holds. However, if `tlast` is ever passed in from an external source that has applied arithmetic to the time values (e.g., `tlast = max(time) - epsilon` for some reason), this comparison could silently return an empty integer vector, causing downstream failures.

**Mitigation:** `pk.calc.tlast()` returns a value directly from the input `time` vector (it's the last time where `conc > 0`), so floating point equality is preserved in the normal call path. The `missing(tlast)` branch recalculates it fresh.

**Recommendation:** If `tlast` is provided externally, consider using `which.min(abs(time - tlast))` as a robust fallback, or add a check that `tlast %in% time` with a clear error if not.

---

## 4. Half-Life Selection Warning as Side Effect in Mask Construction

**File:** `R/half.life.R`, lines 225–241
**Classification:** Confirmed concern (logic correctness)

```r
mask_best <-
  half_lives_for_selection$lambda.z > 0 &
  if (min.hl.points == 2 & nrow(half_lives_for_selection) == 2) {
    rlang::warn(...)
    TRUE
  } else {
    half_lives_for_selection$adj.r.squared >
      (max(half_lives_for_selection$adj.r.squared, na.rm=TRUE) - adj.r.squared.factor)
  }
```

When the `if` branch evaluates to `TRUE` (the `min.hl.points == 2` edge case), `mask_best` becomes `half_lives_for_selection$lambda.z > 0 & TRUE`, which means **all rows with positive lambda.z are selected** as "best". This could select multiple half-life fits simultaneously. The subsequent assignment `ret[, ret_replacements] <- half_lives_for_selection[mask_best, ret_replacements]` will silently take only the first matching row if multiple are selected (due to scalar assignment semantics).

**Risk:** In the edge case of exactly 2 data points with `min.hl.points = 2`, if both produce valid fits (positive lambda.z), both will match `mask_best`, and the first one will be used. This is probably the desired behavior (use the fit with more points, or the first found), but it's not explicitly documented.

**Recommendation:** After the `mask_best` construction, add an assertion or comment: if `sum(mask_best) > 1`, use `which.max(half_lives_for_selection$lambda.z[mask_best])` or document the tie-breaking rule explicitly.

---

## 5. All-BLQ Sentinel Value `tlast = tfirst + 1` in `clean.conc.blq()`

**File:** `R/cleaners.R`, approximately lines 100–107
**Classification:** Confirmed concern (unclear intent)

When all concentrations are BLQ, the code sets `tlast <- tfirst + 1` as a sentinel. This value (`max(time) + 1`) is beyond the actual time range of the data and is used in subsequent mask operations like `tlast <= ret$time`. If the time range happens to include a value exactly equal to `max(time) + 1` (e.g., extended sampling), the mask could behave unexpectedly.

**Recommendation:** Use `Inf` or a clearly named sentinel constant instead of `tfirst + 1`. Add a comment explaining what this sentinel represents and why it is used.

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
| 1 | `auc_integrate.R` log division by zero | Medium | Likely safe — needs comment/test |
| 2 | `auc_integrate.R` log of zero | Medium | Likely safe — needs comment |
| 3 | `auc_integrate.R` floating point `==` | Low | Likely safe in practice |
| 4 | `half.life.R` warning side effect in mask | Medium | Confirmed concern |
| 5 | `cleaners.R` all-BLQ sentinel value | Medium | Confirmed concern — unclear intent |
| 6 | `pk.calc.simple.R` `|` vs `||` | Low | Style issue |
| 7 | `auc_integrate.R` `%in% 0` pattern | Low | Confirmed concern — semantic clarity |
| 8 | `interpolate.conc.R` duplicate times | Low | Likely safe — needs guard/comment |
| 9 | `unit-support.R` `stopifnot` vector | — | False positive — code is correct |
| 10 | `pk.calc.simple.R` lambda.z = NA | Low | Needs test confirmation |
| 11 | `class-PKNCAconc.R` lazy default | Low | Likely safe |
