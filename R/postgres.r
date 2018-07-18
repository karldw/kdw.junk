

#' EXPLAIN and ANALYZE the current query
#'
#' @param x SQL query to explan and analyze
#' @param ... Unused, included for compatability with [dplyr::explain()]
#' @return x, invisibly
#'
#' Note that this is much more expensive than `explain()`
#' @export
explain_analyze <- function(x, ...) {
    # Just like dplyr::explain, but pipe-able and optionally running EXPLAIN ANALYZE
    force(x)
    stopifnot('tbl_sql' %in% class(x))
    dplyr::show_query(x)
    message("\n")
    exsql <- dbplyr::build_sql(dbplyr::sql('EXPLAIN ANALYZE '), dbplyr::sql_render(x))
    expl_raw <- DBI::dbGetQuery(x$src$con, exsql)
    expl <- paste(expl_raw[[1]], collapse = "\n")

    message("<PLAN>\n", expl)
    invisible(x)
}


#' Create an index
#'
#' @param con A DBI connection to a postgresql database
#' @param table_name The name of a table in the postgresql database
#' @param indexed_col Columns or an expression on columns to index
#' @param unique_index Force index to be unique?
#' @param drop_existing Drop index if it already exists?
#' @return The name of the created index
#'
#' Note, postgres is smart enough that you don't need to index a column that's
#' already unique, but if you want to ALTER TABLE to make a primary key, you
#' have to start with a unique index.
#'
#' @export
pg_add_index <- function(con, table_name, indexed_col, unique_index = FALSE,
                         drop_existing = FALSE) {
  # This function is here so I don't have to remember the SQL index syntax and so I
  # don't do anything too dumb. However, it definitely isn't safe or sanitized.
  # Obviously don't expose it to anyone malicious.

  stopifnot(length(table_name) == 1, length(indexed_col) >= 1)
  # Doesn't work with temp tables.  Instead, just reply on SQL to complain.
  # .pg_assert_existence(con, table_name, indexed_col)
  indexed_col_names <- gsub(".", "_", make.names(indexed_col), fixed = TRUE)
  index_name <- paste0(paste(indexed_col_names, collapse = "_"), "_index")
  index_name <- DBI::dbQuoteIdentifier(con, gsub("_+", "_", index_name, perl = TRUE))

  if (unique_index) {
    unique_cmd <- "UNIQUE"
  } else {
    unique_cmd <- ""
  }
  if (drop_existing) {
    drop_cmd <- sprintf("DROP INDEX IF EXISTS %s", index_name)
    DBI::dbSendStatement(con, drop_cmd)
  }
  # If there are multiple columns, make a comma-separated list
  indexed_col_str <- paste(indexed_col, collapse = ", ")
  # fillfactor to 100 because I'm never adding rows to this table
  # TODO: It would be better to use dbBind than this sprintf nonsense
  add_cmd <- sprintf("CREATE %s INDEX %s on %s (%s) WITH (fillfactor = 100)",
                     unique_cmd,
                     index_name,
                     DBI::dbQuoteIdentifier(table_name),
                     DBI::dbQuoteLiteral(indexed_col_str))
  res <- DBI::dbSendStatement(con, add_cmd)
  stopifnot(DBI::dbHasCompleted(res))
  return(index_name)
}


#' Assert the existance of a table, and optionally, columns
#'
#' @param con A DBI connection to a postgresql database
#' @param table_name The name of a table in the postgresql database
#' @param col_name Column names to check. Default doesn't check any.
#' @return Nothing
.pg_assert_existence <- function(con, table_name, col_name = NULL) {
  if (! DBI::dbExistsTable(con, table_name)) {
    err_msg <- sprintf("Table name '%s' is not in the database", table_name)
    stop(err_msg)
  }
  if (! is.null(col_name)) {
    known_cols <- DBI::dbListFields(con, table_name)
    if(! all(col_name %in% known_cols)) {
      unknown_cols <- setdiff(col_name, known_cols)
      column_columns <- if (length(unknown_cols) > 1) "Columns" else "Column"
      unknown_cols_str <- vec2string(unknown_cols)
      err_msg <- sprintf("%s %s not found in table '%s'.",
                         column_columns, unknown_cols_str, table_name)
      stop(err_msg)
    }
  }
  invisible()
}

#' Run the VACUUM command, and maybe ANALYZE too.
#'
#' @param con A DBI connection to a postgresql database
#' @param table_name The name of a table in the postgresql database (
#'   (The default processes all tables in the database)
#' @param analyze Run ANALYZE too? (Default TRUE)
#' @return Nothing
#'
#' @export
pg_vacuum <- function(con, table_name = NULL, analyze = TRUE) {
  if (analyze) {
    sql_cmd <- "VACUUM FREEZE ANALYZE"
  } else {
    sql_cmd <- "VACUUM FREEZE"
  }
  if (!is.null(table_name)) {
    stopifnot(length(table_name) == 1)
    # default w/o table name is all tables in database
    .pg_assert_existence(con, table_name)
    sql_cmd <- paste(sql_cmd, DBI::dbQuoteIdentifier(table_name))
  }
  # TODO: It would be better to use dbBind than this paste nonsense
  res <- DBI::dbSendStatement(con, sql_cmd)
  stopifnot(DBI::dbHasCompleted(res))
}


#' Add a primary key
#'
#' @param con A DBI connection to a postgresql database
#' @param table_name The name of a table in the postgresql database
#' @param key_col The column to use as the primary key
#' @return Nothing
#'
#' @export
pg_add_primary_key <- function(con, table_name, key_col) {
  stopifnot(length(table_name) == 1, length(key_col) >= 1)
  existing_index <- pg_add_index(con, table_name, key_col, unique_index = TRUE)
  # TODO: It would be better to use dbBind than this sprintf nonsense
  sql_cmd <- sprintf("ALTER TABLE %s ADD PRIMARY KEY USING INDEX %s",
                     DBI::dbQuoteIdentifier(table_name),
                     DBI::dbQuoteIdentifier(existing_index))
  res <- DBI::dbSendStatement(con, sql_cmd)
  stopifnot(DBI::dbHasCompleted(res))
}


#' Add a foreign key
#'
#' @param con A DBI connection to a postgresql database
#' @param table_name The name of a table in the postgresql database
#' @param column_name The column to put the foreign key constraint on
#' @param reftable The name of the table providing the foreign key constraint
#' @param refcolumn The name of the column providing the foreign key constraint
#' @return Nothing
#'
#' @export
pg_add_foreign_key <- function(con, table_name, column_name, reftable, refcolumn) {
  .pg_assert_existence(con, table_name, column_name)
  .pg_assert_existence(con, reftable, refcolumn)
  # TODO: It would be better to use dbBind than this sprintf nonsense
  sql_cmd <- sprintf("ALTER TABLE %s ADD FOREIGN KEY (%s) REFERENCES %s (%s)",
                     DBI::dbQuoteIdentifier(table_name),
                     DBI::dbQuoteIdentifier(column_name),
                     DBI::dbQuoteIdentifier(reftable),
                     DBI::dbQuoteIdentifier(refcolumn))
  res <- DBI::dbSendStatement(con, sql_cmd)
  stopifnot(DBI::dbHasCompleted(res))
}
