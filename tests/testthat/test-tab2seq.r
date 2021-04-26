test_that("basic usage is working", {
    tab = matrix(c("a","b","c","d"), 2, 2)
    seq = c("ac", "bd")
    
    expect_identical(seq, tab2seq(tab))
    expect_identical(tab, seq2tab(seq))
    })


test_that("names are preserved", {
    expect_identical(sequences, tab2seq(seq2tab(sequences)))
    })
