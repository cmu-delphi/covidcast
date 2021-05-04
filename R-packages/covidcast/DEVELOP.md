# covidcast development guide

We follow a Git Flow model for development. The `main` branch is for the
"release" version of the package, since changes on `main` are immediately
available for installation and immediately appear on the documentation website.
New features are instead developed on the `r-pkg-devel` branch so they can be
beta-tested before release.

To make a change to the package, pull the latest version of `r-pkg-devel` and
make a branch starting from there. When you open a pull request, set the base
branch to be `r-pkg-devel`, *not* `main`.

A short checklist for submitting pull requests:

1. Run the unit tests with `devtools::test()` and ensure they pass.
2. If you have added any new features (new functions, new options, etc.), add a
   brief description to `NEWS.md` to the next listed version number. Also ensure
   that new functions or datasets are listed in the reference in `_pkgdown.yml`
   so they appear in a good place in the documentation website.
3. Some of the vignettes take too long to re-build on CRAN, so they have been
   precompiled. They are stored with a file name ".Rmd.orig" and converted into
   an ".Rmd" that has no running R code. These should be rebuilt to test changes
   when updating the package, which can be done with the script precompile.R
   found in the vignette directory.
4. If you changed any documentation, rebuild the documentation with
   `devtools::document()` and then `pkgdown::build_site()`. (This can be slow,
   because our vignettes take a long time to build.)
5. Submit the pull request, setting the base branch to `r-pkg-devel`, and see if
   the CI can also successfully run the tests.

## Unit tests

Unit tests are provided in `tests/testthat/` and use the [testthat
package](https://testthat.r-lib.org/). Tests are automatically run as part of `R
CMD check`, but during package development, the easiest way to run all tests is
via `devtools::test()` from within the package working directory.

Our continuous integration (CI) on GitHub will automatically run the unit tests
when you open a pull request, as part of running `R CMD check`. Failed tests and
check errors will both result in the build failing and errors being visible in
the pull request.

### Testing plots

We test our plots and maps with the [vdiffr](https://github.com/r-lib/vdiffr)
package, which renders plots and then visually compares them to a saved
reference image.

See `tests/testthat/test-plot.R` for examples. Each test case uses
`vdiffr::expect_doppelganger` with two arguments: a name (which will be the
filename of the saved plot, so ensure this is descriptive and unique) and a
plot, such as a ggplot object.

When you add new test case or change how a plotting feature works, you will need
to "validate" the tests, meaning vdiffr will render the plots and ask you if
they look correct. To do this, run `vdiffr::manage_cases()` from within the
package working directory. A Shiny app will open up and present you with each
plot. For new plots, it will simply show you the plot, and a "Validate" button
will let you indicate that the plot looks good. For changed plots, it will show
you the difference between the saved image and the new plot. You must ensure
that the changes look correct, then press Validate.

Once you've validated all changes, press Quit and you will be able to commit the
changed images and run the tests successfully.

### Using data files in tests

Unit tests should not depend on being able to load data from the COVIDcast API,
since that data is subject to change. It is preferable to either use toy
examples or to store static datasets to be loaded and used.

Small static datasets can be kept in `tests/testthat/data/` in RDS form (using
`saveRDS` and `loadRDS`). The `testthat::test_path` function locates files
relative to the `tests/testthat/` directory regardless of your current working
directory, so for example you can use

```r
foo <- readRDS(test_path("data/foo.rds"))
```

to load `tests/testthat/data/foo.rds`.

## Documentation

We use
[roxygen2](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html)
and [pkgdown](https://pkgdown.r-lib.org/index.html) to build good-looking
documentation automatically. We should strive to have clear documentation for
all exported functions and data, as well as vignettes giving examples of all
major package features.

After changing a vignette or documentation, you'll need to rebuild the
documentation. Use `devtools::document()` to do this. Then
`pkgdown::build_site(".")` (from within the package directory) will rebuild the
HTML documentation site.

## Release checklist

To release a new public version of the package, copy the template below into a
new GitHub Issue and check off the items one at a time.

```
- [ ] Increment the version number in `DESCRIPTION`
- [ ] Update `NEWS.md` to describe all new features, bug fixes, and breaking changes since the last version
- [ ] Run `devtools::check()` and ensure there are no errors.
- [ ] Run `devtools::document()` and then `pkgdown::build_site()`.
- [ ] Browse the generated documentation site and ensure there are no problems.
- [ ] Commit the updated HTML to `r-pkg-devel`. Make sure you don't miss any new plot files!
- [ ] Open a pull request to merge `r-pkg-devel` to `main` and request review.
```

Once the pull request is merged, the new version is live.
