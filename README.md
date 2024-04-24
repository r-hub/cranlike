
# cranlike

> Tools for CRAN-like Repositories

<!-- badges: start -->
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://www.r-pkg.org/badges/version/cranlike)](https://www.r-pkg.org/pkg/cranlike)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/cranlike)](https://www.r-pkg.org/pkg/cranlike)
[![R-CMD-check](https://github.com/r-hub/cranlike/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-hub/cranlike/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/r-hub/cranlike/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-hub/cranlike?branch=main)
<!-- badges: end -->

A set of functions to manage CRAN-like repositories efficiently.
This package is an alternative to `tools::write_PACKAGES`.
The goal is to make updates to the repository easier and faster,
without the need of scanning all the package files.

`cranlike` keeps the package data in a SQLite database, in addition
to the `PACKAGES*` files. This database is the canonical source of the
package data. It can be updated quickly, to add and remove packages.
The `PACKAGES*` files are generated from the database.

## Installation

```r
install.packages("cranlike")
```

## Usage

```r
library(cranlike)
```

### Setting up a repository

`create_empty_PACKAGES` creates an empty CRAN-like repository in the
specified directory. It creates the SQLite database (if it does not exist),
and also the `PACKAGES*` files.

`update_PACKAGES` is similar, but it also scans the directory for package
files, and adds them to the database. Use this function on an existing
CRAN-like repository. It creates and updates the database, and then
the `PACKAGES*` files.

### Adding and removing packages

`add_PACKAGES` adds one or more package files to the repository.
The files must already exist in the directory. The database is created
if needed, and then updated with the new packages. Then the `PACKAGES*`
files will be re-generated.

`remove_PACKAGES` removes one or more package files from the repository.
It first removes them from the database, and then removes the files from
the directory. Finally, it re-generates the `PACKAGES*` files.

### Listing packages

`package_versions` lists all packages in the repository. It uses the
SQLite database instead of parsing the `PACKAGES*` files, so it is much
faster.

## License

GPL Version 2 or higher © R Consortium
