test_that("convert between vectors types", {
    foo = 1:4
    bar = as.numeric(foo)
    
    expect_identical(convert(foo, "double"), bar)
    
    foo = letters[1:4]
    expect_warning(convert(foo, "double"), "NAs introduced by coercion")
    
    foo = 1:4
    bar = as.character(1:4)
    expect_identical(convert(foo, "character"), bar)
    })


test_that("convert works for shallow lists", {
    foo = list(1,2,3,4)
    bar = list("1","2","3","4")
    expect_identical(convert(foo, "character"), bar)
    })


test_that("convert works for deep lists", {
    foo = list(1, 2, list(3, 4, 5))
    bar = list("1", "2", list("3", "4", "5"))
    expect_identical(convert(foo, "character"), bar)
    })


test_that("convert works for matrices", {
    foo = matrix(1:4, 2, 2)
    bar = matrix(as.character(1:4), 2, 2)
    expect_identical(convert(foo, "character"), bar)
    })


test_that("convert works for data.frames", {
    foo = data.frame("a"=1:4, "b"=5:8)
    bar = data.frame("a"=as.character(1:4), "b"=as.character(5:8))
    expect_identical(convert(foo, "character"), bar)
    
    foo = data.frame("a"=1:4, "b"=letters[1:4])
    bar = data.frame("a"=as.character(1:4), "b"=letters[1:4])
    expect_identical(convert(foo, "character"), bar)
    })
