test_that("replace numeric variables", {
    foo = matrix(1:4, 2, 2)
    bar = matrix(2:5, 2, 2)
    expect_equal(replace(foo, 1:4, 2:5), bar)
    })


test_that("replace character variables", {
    foo = c("foo", "bar", "baz")
    bar = c("baz", "bar", "foo")
    expect_equal(replace(foo, c("foo", "bar", "baz"), c("baz", "bar", "foo")), bar)
    })


test_that("can replace only a single variable", {
    foo = c("foo", "bar", "baz")
    bar = c("foo", "bar", "spam")
    expect_equal(replace(foo, "baz", "spam"), bar)

    foo = matrix(1:4, 2, 2)
    bar = foo
    bar[1,1] = 5
    expect_equal(replace(foo, 1, 5), bar)    
    })


test_that("throws error when the length of values and replace differs", {
    foo = c("foo", "bar", "baz")
    expect_error(replace(foo, foo, "bar"))
    })


test_that("rescale ordinal scale", {
    foo = c(1, 3, 5)
    bar = c(1, 2, 3)
    expect_equal(replace.ordinal(foo), bar)
    })
