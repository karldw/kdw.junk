context("testing postgresql convenience functions")


can_con <- function(con) {
  inherits(con, "PqConnection")
}

pg_con <- NULL
test_that("check utils", expect_false(can_con(pg_con)))

# Try to connect to the pudl_test database, because that's what I have around.
try(pg_con <- DBI::dbConnect(RPostgres::Postgres(),
  user = "catalyst", dbname = "pudl_test", host = "127.0.0.1"), silent=TRUE)


test_that("pg_add_index adds an index", {
  skip_if_not(can_con(pg_con), "could not connect to postgres database")
  # Write the iris data to a temporary table
  DBI::dbWriteTable(pg_con, "iris", iris, temporary=TRUE, overwrite=TRUE)
  # Also write the iris data, but with names as underscores
  iris_underscore <- iris
  colnames(iris_underscore) <- c("Sepal_Length", "Sepal_Width", "Petal_Length",
    "Petal_Width", "Species")
  DBI::dbWriteTable(pg_con, "iris_underscore", iris_underscore, temporary=TRUE, overwrite=TRUE)
  index_name_iris            <- pg_add_index(pg_con, "iris",            "Sepal.Width")
  index_name_iris_underscore <- pg_add_index(pg_con, "iris_underscore", "Sepal_Length")

  index_query <- "
    select
      t.relname as table_name,
      i.relname as index_name,
      a.attname as column_name
    from
      pg_class t,
      pg_class i,
      pg_index ix,
      pg_attribute a
    where
      t.oid = ix.indrelid
      and i.oid = ix.indexrelid
      and a.attrelid = t.oid
      and a.attnum = ANY(ix.indkey)
      and t.relkind = 'r'
      and t.relname like 'iris%'
    order by
      t.relname,
      i.relname;
  "
  index_info <- DBI::dbGetQuery(pg_con, index_query)
  DBI::dbDisconnect(pg_con)
  expected_indexes <- tibble::tribble(
    ~table_name,       ~index_name,          ~column_name,
    "iris",            "Sepal_Width_index",  "Sepal.Width",
    "iris_underscore", "Sepal_Length_index", "Sepal_Length"
  )
  expect_equal(index_info, expected_indexes, check.attributes = FALSE)
})
