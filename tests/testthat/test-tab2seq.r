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
