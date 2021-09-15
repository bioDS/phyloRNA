test_that("replace numeric variables", {
    foo = matrix(1:4, 2, 2)
    bar = matrix(2:5, 2, 2)
    expect_identical(replace(foo, 1:4, 2:5), bar)
    })


test_that("replace character variables", {
    foo = c("foo", "bar", "baz")
    bar = c("baz", "bar", "foo")
    expect_identical(replace(foo, c("foo", "bar", "baz"), c("baz", "bar", "foo")), bar)
    })


test_that("can replace only a single variable", {
    foo = c("foo", "bar", "baz")
    bar = c("foo", "bar", "spam")
    expect_identical(replace(foo, "baz", "spam"), bar)

    foo = matrix(1:4, 2, 2)
    bar = foo
    bar[1,1] = 5
    expect_identical(replace(foo, 1, 5), bar)    
    })


test_that("can replace values in a data.frame", {
    foo = data.frame("foo"=c(1,2), "bar"=c(3,4))
    bar = data.frame("foo"=c(2,3), "bar"=c(4,5))
    expect_identical(replace(foo, 1:4, 2:5), bar)
    })

test_that("throws error when the length of values and replace differs", {
    foo = c("foo", "bar", "baz")
    expect_error(replace(foo, foo, "bar"))
    })


test_that("rescale ordinal scale", {
    foo = c(1, 3, 5)
    bar = c(1, 2, 3)
    expect_identical(replace_ordinal(foo), bar)
    
    foo = c(1, 5, 3)
    bar = c("a", "c", "b")
    expect_identical(replace_ordinal(foo, letters), bar)
    })


test_that("replace_ordinal preserve type and works for vector", {
    # integer
    foo = c(1L, 3L, 1L, 5L)
    bar = c(1L, 2L, 1L, 3L)
    expect_identical(replace_ordinal(foo), bar)
    
    # double
    foo = c(1, 3, 1, 5)
    bar = c(1, 2, 1, 3)
    expect_identical(replace_ordinal(foo), bar)
    
    # character
    foo = c("a", "b", "a", "c")
    bar = c("1", "2", "1", "3")
    expect_identical(replace_ordinal(foo), bar)
    })


test_that("replace_ordinal preserves type and works for list", {
    # integer
    foo = list(1L, 3L, 1L, 5L)
    bar = list(1L, 2L, 1L, 3L)
    expect_identical(replace_ordinal(foo), bar)
    
    # double
    foo = list(1, 3, 1, 5)
    bar = list(1, 2, 1, 3)
    expect_identical(replace_ordinal(foo), bar)
    
    # character
    foo = list("a", "b", "a", "c")
    bar = list("1", "2", "1", "3")
    expect_identical(replace_ordinal(foo), bar)
    })


test_that("replace_ordinal preserve type and works for matrix", {
    # integer
    foo = matrix(c(1L, 3L, 1L, 5L), 2, 2)
    bar = matrix(c(1L, 2L, 1L, 3L), 2, 2)
    expect_identical(replace_ordinal(foo), bar)
    
    # double
    foo = matrix(c(1, 3, 1, 5), 2, 2)
    bar = matrix(c(1, 2, 1, 3), 2, 2)
    expect_identical(replace_ordinal(foo), bar)
    
    # character
    foo = matrix(c("a", "b", "a", "c"), 2, 2)
    bar = matrix(c("1", "2", "1", "3"), 2, 2)
    expect_identical(replace_ordinal(foo), bar)
    })



test_that("replace_ordinal preserve type and works for data.frame", {
    # integer
    foo = as.data.frame(matrix(c(1L, 3L, 1L, 5L), 2, 2))
    bar = as.data.frame(matrix(c(1L, 2L, 1L, 3L), 2, 2))
    expect_identical(replace_ordinal(foo), bar)
    
    # double
    foo = as.data.frame(matrix(c(1, 3, 1, 5), 2, 2))
    bar = as.data.frame(matrix(c(1, 2, 1, 3), 2, 2))
    expect_identical(replace_ordinal(foo), bar)
    
    # character
    foo = as.data.frame(matrix(c("a", "b", "a", "c"), 2, 2))
    bar = as.data.frame(matrix(c("1", "2", "1", "3"), 2, 2))
    expect_identical(replace_ordinal(foo), bar)
    })
