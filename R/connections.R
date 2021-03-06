#' Test a DBI connection
#' @export
dbtTestCon <- function(dbcon) {
  return(
    try(
      dbGetQuery(conn      = dbcon,
                 statement = "SELECT 1 FROM DUAL"), # todo: some more universal select for any type db?
      silent = TRUE)
    == 1)
}


#' Check whether .dbcon environment (for storing connections) exists
#' @export
dbtEnvExists <- function(){
  if (exists(".dbcon", envir = .GlobalEnv)) return(TRUE)
  return(FALSE)
}


#' Check whether a connections exists
dbtConExists <- function(con_name) {
  if (dbtEnvExists() && exists(con_name, envir = .GlobalEnv$.dbcon)) return(TRUE)
  return(FALSE)
}

#' Is the connection open?
dbtConOpen <- function(con_name, dbname) {
  if (dbtEnvExists() &&
      dbtConExists(con_name) &&
      tryCatch(dbGetInfo(.GlobalEnv$.dbcon[[con_name]]),
                error = function(e) return(list(dbname = "")))$dbname == dbname)
    return(TRUE)
  # else:
  return(FALSE)
}

#' Create connection checker
#' @export
dbtConChecker <- function(con_name, dbname) {
  function() {
    dbtConOpen(con_name, dbname)
  }
}

# Create function that checks for an existing DB connection and creates/open a new one if its not found open
#' @export
dbtConnector <- function(con_name,
                         def_driver,
                         checker,
                         def_args = NULL) {

  function(...,
           drv = def_driver) {

    if (dbtEnvExists()) {
      if (checker()) return(invisible(TRUE))
    } else {
      .GlobalEnv$.dbcon <- new.env()
    }

    # arguments:
    con_args <- list(drv = drv)
    # user's new arguments
    users_args <- list(...)
    con_args <- append(con_args, users_args)
    # developer's default arguments if not in conflict
    # todo: prevent conflicts by matching (the same way R does)
    if (!is.null(def_args)) con_args <- append(con_args, def_args[!(names(def_args) %in% names(users_args))])

    # try to connect
    # todo: exceptions
    tryCatch(new_con <- do.call(dbConnect, args = con_args),
             error = function(e) stop("Error while connecting to ", con_name, ": ", e))

    # save it to a known environment
    .GlobalEnv$.dbcon[[con_name]] <- new_con

    # exit
    return(invisible(TRUE))
  }
}

# todo: more granular testing: 1. environment exists 2. connection exists 3. connection is open/closed
#' Create DBI disconnetor
#' @export
dbtDisconnector <- function(con_name, checker) {
  function() {
    if (dbtEnvExists()) {
      if (dbtConExists(con_name)) {
        if (checker()) {
          message("Closing connection to ", con_name, " ...")
          try(dbDisconnect(conn = .GlobalEnv$.dbcon[[con_name]]))
          message("ok")

          return(TRUE)
        } else {
          message("Connection to ", con_name, " already closed.")
          return(TRUE)
        }
      } else {
        message("Connection to ", con_name, " not found!")
        return(FALSE)
      }
    } else {
      message("Connection to ", con_name, " not found!")
      return(FALSE)
    }
  }
}
