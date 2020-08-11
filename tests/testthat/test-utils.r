local_dir = function(dir, create=FALSE, env=parent.frame()){
    if(create)
        dir.create(dir)
    withr::defer(unlink(dir, recurisve=TRUE, force=TRUE), envir=env)
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
    expect_warning(dir.create(tmpdir))
    expect_silent(mkdir(tmpdir))
    })


test_that("extracting path core with corename", {
    expect_equal(corename("foo/bar.baz"), "bar")
    expect_equal(corename("foo/bar"), "bar")
    expect_equal(corename("bar.baz"), "bar")
    expect_equal(corename("foo.bar.baz"), "foo.bar")
    })


test_that("test abspath when path exists", {
    tmpdir = "."
    tmpfile = tempfile(tmpdir=".")
    local_file(tmpfile, create=TRUE)
    expect_true(file.exists(tmpfile))
    expect_equal(abspath(tmpdir), tools::file_path_as_absolute(tmpdir))
    expect_equal(abspath(tmpfile), tools::file_path_as_absolute(tmpfile))
    })


test_that("absbpath when path does not exists", {
    tmpdir = file.path(".", "foo")
    tmpfile = tempfile(tmpdir=".")
    expect_error(tools::file_path_as_absolute(tmpdir))
    expect_error(tools::file_path_as_absolute(tmpfile))
    expect_equal(abspath(tmpdir), file.path(getwd(), "foo"))
    expect_equal(abspath(tmpfile), file.path(getwd(), basename(tmpfile)))
    })
