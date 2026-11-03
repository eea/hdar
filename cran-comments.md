## Submission

This is a bug-fix release (1.0.8).

## Changes

- Fixed `generate_query_template()` failing for datasets whose queryable metadata
  contains array parameters without `oneOf`/`enum` (e.g. CDSE-hosted datasets),
  which previously raised `attempt to set an attribute on NULL`.
- Added support for array `enum` and for `integer`, `boolean`, unknown and
  missing parameter types.
- Removed a stale `humanize` import from `NAMESPACE` (unused, and `humanize` is
  not a dependency) and added the missing `progress` import.
- Updated stale dataset IDs in tests and added regression tests for the above.

## R CMD check results

0 errors | 0 warnings | 1 note

```
* checking CRAN incoming feasibility ... NOTE
Found the following (possibly) invalid URLs:
  URL: https://www.wekeo.eu/
    From: DESCRIPTION
    Status: 429
    Message: Too Many Requests
  URL: https://www.wekeo.eu/data?view=viewer
    From: inst/doc/hdar.html
    Status: 429
    Message: Too Many Requests
  URL: https://www.wekeo.eu/register
    From: inst/doc/hdar.html
    Status: 429
    Message: Too Many Requests
```

The URLs are valid; the HTTP 429 responses are rate limiting applied to
automated (non-browser) requests. They load normally in a browser.

## Notes

- The package's `testthat` suite exercises the live WEkEO HDA API and requires
  credentials, so it is not run during `R CMD check` (there is no
  `tests/testthat.R`).
- Checked locally on Ubuntu 24.04 with R 4.6.1.

## Test environments

- local: Ubuntu 24.04.5 LTS, R 4.6.1 (x86_64-pc-linux-gnu)
