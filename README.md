# kdw.junk

Miscellaneous R functions Karl wanted to reuse.  Inspired by [kimisc](https://github.com/krlmlr/kimisc/).

For a similar grab-bag of Python and Stata scripts, see https://github.com/karldw/useful_scripts.

## Installation

```r
devtools::install_github("karldw/kdw.junk")
```

## Features

### Where is my Dropbox / Box folder?
- `box_home` -- Find Box
- `dropbox_home` -- Find Dropbox

### Are my data uniquely identified?
- `is_id` -- Do variables uniquely ID rows? (Works with `dplyr::select` syntax).
- `ensure_id_vars` -- Ensure variables uniquely ID rows (works with `dplyr::select` syntax; useful in pipelines).

### Parallel convenience functions
- `get_cores` -- Get number of cores available, with a reasonable max.
- `lapply_bind_rows` -- Like `purrr::map_df`, but runs in parallel and can handle `sf` data.
- `lapply_parallel` -- Wrapper around `mclapply`; works (somewhat inefficiently) on Windows.

### Minor convenience functions
- `clear_all` -- Delete everything in session.
- `get_os` -- Get current OS.
- `install_lazy` -- Install packages if necessary.
- `is.connection` -- Test if object is a connection
- `remove_ext` -- Remove extension (like `file_path_sans_ext`, but works if the 'file' is actually a connection).
- `month_to_quarter` -- Convert months to quarters.
- `stopif` -- Inverse of `stopifnot`.
- `save_plot` -- Save `ggplot` graphic with aspect ratio more appropriate for slides or letter paper (uses `cairo_pdf` for better graphics).

### Smooth out tidyverse bumps
- `make_join_safer` -- Take a `dplyr::*_join` function and return a new function that performs additional checks to avoid cartesian products.
- `read_dta` -- Wrapper around `haven::read_dta` to fix hassles with attributes and factors.
