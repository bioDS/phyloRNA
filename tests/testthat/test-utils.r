local_dir = function(dir, create=FALSE, env=parent.frame()){
    if(create)
        dir.create(dir)
    withr::defer(unlink(dir, recursive=TRUE, force=TRUE), envir=env)
    }


local_file = function(file, create=FALSE, env=parent.frame()){
    if(create)
        file.create(file)
    withr::defer(file.remove(file), envir=env)
    }


test_that("mkdir does not throw warning if the path exists", {
    tmpdir = tempdir(check=TRUE)
    expect_warning(dir.create(tmpdir))
    expect_silent(mkdir(tmpdir))
    })


test_that("mkdir creates a folders recursively", {
    tmpdir = tempdir(check=TRUE)
    tmpdir2 = file.path(tmpdir, "foo")
    tmpdir3 = file.path(tmpdir2, "bar")
    local_dir(tmpdir2)
    expect_warning(dir.create(tmpdir3))
    expect_silent(mkdir(tmpdir3))
    })


test_that("extracting path core with corename", {
    expect_equal(corename("foo/bar.baz"), "bar")
    expect_equal(corename("foo/bar"), "bar")
    expect_equal(corename("bar.baz"), "bar")
    expect_equal(corename("foo.bar.baz"), "foo.bar")
    })


test_that("abspath works when path exists", {
    tmpdir = "."
    tmpfile = tempfile(tmpdir=".")
    local_file(tmpfile, create=TRUE)
    expect_true(file.exists(tmpfile))
    expect_equal(abspath(tmpdir), tools::file_path_as_absolute(tmpdir))
    expect_equal(abspath(tmpfile), tools::file_path_as_absolute(tmpfile))
    })


test_that("absbpath works when path does not exists", {
    tmpdir = file.path(".", "foo")
    tmpfile = tempfile(tmpdir=".")
    expect_error(tools::file_path_as_absolute(tmpdir))
    expect_error(tools::file_path_as_absolute(tmpfile))
    expect_equal(abspath(tmpdir), file.path(getwd(), "foo"))
    expect_equal(abspath(tmpfile), file.path(getwd(), basename(tmpfile)))
    })


test_that("is_nn recognize NULL and NA", {
    expect_true(is_nn(NULL))
    expect_true(is_nn(NA))
    expect_true(is_nn(c(NA, NA)))
    
    expect_false(is_nn(c(1, NA)))
    expect_false(is_nn(c("a", NA)))
    })


test_that("missing_to_na transform a custom missing value to the NA", {
    # vector
    foo = c(0, 1, 2)
    bar = c(NA, 1, 2)
    expect_identical(missing_to_na(foo, 0), bar)
    
    foo = c("N/A", "foo", "bar")
    bar = c(NA, "foo", "bar")
    expect_identical(missing_to_na(foo, "N/A"), bar)
    
    foo = c(NA, 1, 2)
    bar = c(NA, "foo", "bar")
    expect_identical(missing_to_na(foo, NA), foo)
    expect_identical(missing_to_na(bar, NA), bar)

    # list
    foo = list(0, 1, 2)
    bar = list(NA, 1, 2)
    expect_identical(missing_to_na(foo, 0), bar)
    
    foo = list("N/A", "foo", "bar")
    bar = list(NA, "foo", "bar")
    expect_identical(missing_to_na(foo, "N/A"), bar)
    
    # list, but without recursion
    foo = list(c("N/A", "foo", "bar"), "N/A", "foo", "bar")
    bar = list(c("N/A", "foo", "bar"), NA, "foo", "bar")
    expect_identical(missing_to_na(foo, "N/A"), bar)

    # matrix
    foo = matrix(1:4, 2, 2)
    bar = foo
    bar [1,1] = NA
    expect_identical(missing_to_na(foo, 1), bar)

    # data.frame
    foo = as.data.frame(foo)
    bar = as.data.frame(bar)
    expect_identical(missing_to_na(foo, 1), bar)
    })


test_that("all_files_exist works", {
    files = replicate(4, tempfile())

    expect_false(all_files_exist(files))

    file.create(files[1])
    expect_false(all_files_exist(files))

    file.create(files)
    expect_true(all_files_exist(files))

    unlink(files)
    })
