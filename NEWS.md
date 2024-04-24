# cranlike 1.0.3

* cranlike now adds the size of the file to the metadata, in the `Filesize`
  column.

* cranlike can now add custom metadata. It has to be scalar currently,
  so it will be the same for all packages added at the same time.

# cranlike 1.0.2

* `create_empty_PACKAGES()`, `add_PACKAGES()`, `update_PACKAGES()` and
  `remove_PACKAGES()` lock the DB, to avoid potential concurrency issues.

# cranlike 1.0.1

* `package_versions()` can list extra columns from the database now

# cranlike 1.0.0

First public release.
