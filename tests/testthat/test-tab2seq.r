test_that("converts matrix to seqences and back", {
    tab = matrix(c("a","b","c","d"), 2, 2)

    seq = c("ac", "bd")
    expect_identical(seq, tab2seq(tab))
    expect_identical(tab, seq2tab(seq))

    seq = c("ab", "cd")
    expect_identical(seq, tab2seq(tab, margin=2))
    expect_identical(tab, seq2tab(seq, margin=2))
    })


test_that("converts data.frame to sequences", {
    tab = data.frame("foo"=c("a","b"), "bar"=c("c","d"))

    seq = c("ac", "bd")
    expect_identical(seq, tab2seq(tab))

    seq = c("foo"="ab", "bar"="cd")
    expect_identical(seq, tab2seq(tab, margin=2))
    })


test_that("names are preserved after conversions", {
    expect_identical(sequences, tab2seq(seq2tab(sequences)))
    })


test_that("error is reported when the elements in matrix have length greater than 1", {
    tab = matrix(c("aa","bb","cc","dd"), 2, 2)
    expect_error(tab2seq(tab))
    })


test_that("nchardf throws errow with unsupported format", {
    expect_error(nchardf(list("a","b","c")))
    })


test_that("error is reported when margin is not 1 or 2", {
    seq = c("ab", "cd")
    tab = matrix(c("a","c","b","d"), 2, 2)

    expect_error(seq2tab(seq, c(1,2)))
    expect_error(seq2tab(seq, 3))
    expect_error(seq2tab(seq, "foo"))
    expect_error(seq2tab(seq, c("foo", "bar")))
    
    # Valid if objects can be converted to numeric
    expect_identical(seq2tab(seq, list(1)), tab)
    expect_identical(seq2tab(seq, "1"), tab)
    expect_identical(seq2tab(seq, list("1")), tab)
    })
