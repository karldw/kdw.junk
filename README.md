# kdw.junk

<!-- badges: start -->
 [![R build status](https://github.com/karldw/kdw.junk/workflows/R-CMD-check/badge.svg)](https://github.com/karldw/kdw.junk/actions)
<!-- badges: end -->

Miscellaneous R functions Karl wanted to reuse.  Inspired by [kimisc](https://github.com/krlmlr/kimisc/).

For a similar grab-bag of Python and Stata scripts, see https://github.com/karldw/useful_scripts.

## Installation

```r
remotes ::install_github("karldw/kdw.junk")
```

## Features

### Where is my Dropbox / Box folder?
- `box_home` – Find Box
- `dropbox_home` – Find Dropbox

### Are my data uniquely identified?
- `is_id` – Do variables uniquely ID rows? (Works with `dplyr::select` syntax for local and remote tables. For local tables, faster when data.table is installed.).
- `ensure_id_vars` – Ensure variables uniquely ID rows (works with `dplyr::select` syntax; useful in pipelines).

### Plotting

- `binscatter` – Provide a command like binscatter in Stata, based on [binsreg](https://cran.r-project.org/web/packages/binsreg/index.html), but with a formula interface and plotting support for fixed effects.
- `capture_plot_output` – Like `utils::capture.output()`, when the expression would open a graphics device.
- `configure_tikzDevice` – Prepare to use tikzDevice
- `save_plot` – Save `ggplot` graphic with aspect ratio more appropriate for slides or letter paper (uses `cairo_pdf` for better graphics).
    - Offers a `reproducible` option to reset file timestamps.

### Minor convenience functions


- `auto_render` – Render and automatically re-render `rmd`, `md`, or `tex` files.
- `cite_attached_packages` – Generate biblatex citations for the attached packages.
- `clear_all` – Delete everything in session.
- `get_cores` – Get number of cores available, with a reasonable max.
- `get_os` – Get current OS.
- `install_lazy` – Install packages if necessary.
- `is.connection` – Test if object is a connection.
- `make_better_names` – Improve a character vector of names.
- `memory_limit` – Set a memory limit (Linux and Windows only).
- `month_to_quarter` – Convert months to quarters.
- `make_monthly` – Convert date to monthly (day of month set to 1).
- `rename_cols` – Rename columns with a dict-like approach.
- `st_union_intersection` – Union intersecting `sf` geometries.
- `stopif` – Inverse of `stopifnot`.
- `truncate_bytes` – Truncate a string to a particular length in bytes (not very efficiently).
- `winsorize` – [winsorize](https://en.wikipedia.org/wiki/Winsorizing) in a way that handles point masses.


### Smooth out tidyverse bumps

- `read_dta` – Wrapper around `haven::read_dta` to fix hassles with attributes and factors.

### US Holidays

Does what is says on the box.

- `hol_christmas_day`
- `hol_columbus_day`
- `hol_daylight_saving`
- `hol_easter`
- `hol_george_washington_birthday`
- `hol_good_friday`
- `hol_inauguration_day`
- `hol_independence_day`
- `hol_labor_day`
- `hol_martin_luther_king_day`
- `hol_memorial_day`
- `hol_new_years_day`
- `hol_presidents_day`
- `hol_thanksgiving_day`
- `hol_us_federal_holidays`
- `hol_veterans_day`

### Postgres management conveniences

- `explain_analyze`
- `pg_add_foreign_key`
- `pg_add_index`
- `pg_add_primary_key`
- `pg_vacuum`

## Old functions

### Removed functions
- `read_data` – use `rio::import()` instead.
- `lapply_parallel` – use `furrr::future_map()` instead.
- `lapply_bind_rows` – use `furrr::future_dfr()` instead.


### Hard-deprecated functions

- `vec2string` – you're better off using `glue::glue()` most of the time.
- `make_join_safer` – use [safejoin](https://github.com/moodymudskipper/safejoin) instead.
    - Takes a `dplyr::*_join` function and return a new function that performs additional checks to avoid many-to-many merges
- `merge_stata` – use [safejoin](https://github.com/moodymudskipper/safejoin) instead.
    - Defined a merge function that mirrored Stata's `merge`
- `narrate` – use `rlang::inform()` instead.
- `update_packages` – update packages and reinstall everything that depends on Rcpp when Rcpp is updated. Deprecated because that doesn't seem to be an issue anymore.


### Soft-deprecated functions

- `felm_strict` – Run `lfe::felm()` strictly.
    - Use `fixest::feols()` instead
