test_that("Can reads fasta sequences", {
    expect_identical(read_fasta(test_fasta), sequences)
    
    fasta = read_fasta(primates_fasta)
    expect_true( all(nchar(fasta) == 898) )
    })
