count_numbers = function(x){
    nchar(gsub("[^[:digit:]]", "", x))
    }


count_letters = function(x){
    nchar(gsub("[^a-z]", "", x))
    }


test_that("generates a random string", {
    expect_no_match(random_string(8), random_string(8))
    })


test_that("generates the same string when seed is fixe", {
    expect_identical({set.seed(1); random_string(8)}, {set.seed(1); random_string(8)})
    })


test_that("random string has the specified number of letters and numerals", {
    expect_identical(count_letters(random_string(8, 2)), 2L)
    expect_identical(count_letters(random_string(7, 3)), 3L)
    expect_identical(count_letters(random_string(6, 4)), 4L)
    expect_identical(count_letters(random_string(5, 5)), 5L)
    
    expect_identical(count_numbers(random_string(8, numbers=2)), 2L)
    expect_identical(count_numbers(random_string(7, numbers=3)), 3L)
    expect_identical(count_numbers(random_string(6, numbers=4)), 4L)
    expect_identical(count_numbers(random_string(5, numbers=5)), 5L)
    })


test_that("error when the number of letters and numerals is larger than specified length", {
    expect_error(random_string(2, 1, 2))
    expect_error(random_string(2, 0, 3))
    expect_error(random_string(2, 3))
    })
