# icerui (development version)

The package is being written to use S3 objects behind the scenes. When this is complete you will be able to run code like:

```r
library(icerui)

cea1 <- BCEA::bcea(...)
ui1 <- icerui(cea1)
ui1
confint(ui1)
plot(ui1)

cea2 <- heemod::run_psa(...)
ui2 <- icerui(cea2, method = "percentile")
```

You will be able to specify the comparisons you want confidence intervals for the ICERs for in a number of ways.

The formula notation will be the most flexible:

```r
# ICER of treatment vs. control
icerui(x, comparison = treatment ~ control)

# ICERs of trtC vs. trtA and trtC vs. trtB
icerui(y, comparison = trtC ~ list(trtA, trtB))

# ICERs of trtC vs. trtB, trtC vs. trtA and trtB vs. trtA
# (ignores invalid trtB vs. trtB comparison)
icerui(y, comparison = list(trtC, trtB) ~ list(trtB, trtA))

# Same as above
# (. on either side mirrors other side)
icerui(y, comparison = list(trtC, trtB, trtA) ~ .)
```

For BCEA and heemod PSA objects you can also specify comparisons with character vectors, but the behaviour is dependent on whether the input is a BCEA object or heemod PSA object:
* For BCEA objects, `bcea$ref` will be compared to each option in the character vector
* For heemod PSA objects, each strategy listed will be compared to `psa$model$central_strategy`

For BCEA objects you can also specify comparisons by integers instead of strings.

For BCEA and heemod PSA objects if you do not specify a comparison, it will behave as if you asked for all comparisons (`bcea$ref` vs. each in `bcea$comp` or all strategies versus `psa$model$central_strategy`)

## Other changes

* `uiplot` is deprecated in favour of `plot.icerui`
* `fieller`, `bspercent` and `bsaccept` are now exported for direct use (e.g. if you don't have a BCEA or heemod PSA object)
