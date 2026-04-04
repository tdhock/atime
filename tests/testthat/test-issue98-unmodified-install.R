library(testthat)
test_that("atime_versions installs unmodified package for Sexpr Rd calls", {
  tdir <- tempfile()
  dir.create(tdir)
  pkg.path <- file.path(tdir, "testpkg")
  dir.create(pkg.path)
  dir.create(file.path(pkg.path, "R"))
  dir.create(file.path(pkg.path, "man"))
  writeLines(c(
    "Package: testpkg",
    "Title: Test Package for Issue 98",
    "Version: 1.0.0",
    "Description: Minimal package to test atime_versions with Sexpr in Rd.",
    "License: GPL-3"
  ), file.path(pkg.path, "DESCRIPTION"))
  writeLines(c(
    "export(my_fun)",
    "export(rd_helper)"
  ), file.path(pkg.path, "NAMESPACE"))
  writeLines(c(
    "my_fun <- function(x) x + 1",
    "rd_helper <- function() '\\\\code{test}'"
  ), file.path(pkg.path, "R", "functions.R"))
  writeLines(c(
    "\\name{my_fun}",
    "\\alias{my_fun}",
    "\\title{Test Function}",
    "\\description{A test function that uses Sexpr calling original package name.}",
    "\\usage{my_fun(x)}",
    "\\arguments{\\item{x}{A numeric value.}}",
    "\\details{",
    "  \\Sexpr[results=rd,stage=build]{testpkg:::rd_helper()}",
    "}",
    "\\value{The input plus one.}"
  ), file.path(pkg.path, "man", "my_fun.Rd"))
  git2r::init(pkg.path)
  repo <- git2r::repository(pkg.path)
  git2r::add(repo, "*")
  sig <- git2r::default_signature(repo)
  if(is.null(sig)){
    git2r::config(repo, user.name="test", user.email="test@test.com")
  }
  git2r::commit(repo, "initial commit")
  sha <- git2r::revparse_single(repo, "HEAD")$sha
  short.sha <- substr(sha, 1, 7)
  old.lib <- .libPaths()
  new.lib <- file.path(tdir, "library")
  dir.create(new.lib)
  .libPaths(c(new.lib, old.lib))
  on.exit({
    .libPaths(old.lib)
    unlink(tdir, recursive=TRUE)
  }, add=TRUE)
  expect_error(
    atime::atime_versions(
      pkg.path=pkg.path,
      N=c(1, 2),
      setup={x <- N},
      expr=testpkg::my_fun(x),
      test.sha=sha
    ),
    regexp="testpkg|error|install",
    ignore.case=TRUE
  )
})
