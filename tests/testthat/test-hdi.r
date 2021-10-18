test_that("calculated hdi is close to the real one", {
    set.seed(1) # make tests non-random and thus deterministic
    x = rnorm(1e6)
    alpha = 0.05
    expect_equal(hdi(x, alpha=alpha), qnorm(c(alpha/2, 1-alpha/2)), ignore_attr=TRUE, tolerance=0.1)
    
    y = density(x)
    expect_equal(hdi(y, alpha=alpha), qnorm(c(alpha/2, 1-alpha/2)), ignore_attr=TRUE, tolerance=0.1)
    })


test_that("size and alpha are equivalent options", {
    set.seed(1)
    x = rnorm(1e6)
    expect_identical(hdi(x, size=0.5), hdi(x, alpha=0.5))
    expect_identical(hdi(x, size=0.75), hdi(x, alpha=0.25))
    expect_identical(hdi(x, size=0.90), hdi(x, alpha=0.10)) 
    })
