#' DataStoreDriver -- A wrapper to SQLiteDriver
#' 
#' In order to define a dbConnect method for SQLiteDataStore,
#' a custom class derived from SQLiteDriver is needed.
#'
#' This class is for internal purposes only.
#' 
#' @name DataStoreDriver-class
#' @rdname DataStoreDriver-class
.DataStoreDriver <- setClass(
  Class="DataStoreDriver", 
  contains="SQLiteDriver"
)

#' SQLiteDataStore -- A wrapper to SQLiteConnection
#' 
#' SQLiteDataStore provides different \code{dbReadTable} and
#' \code{dbWriteTable} than its ancestor. Further methods
#' for other data types than data.frame may be provided.
#'
#' Usually, objects of this class are created by \code{datastore}.
#' 
#' @name SQLiteDataStore-class
#' @rdname SQLiteDataStore-class
setClass(
  Class="SQLiteDataStore", 
  contains="SQLiteConnection"
)

#' data_store_driver -- a constructor for DataStoreDriver object. 
#'
#' Internal function.
#' Basically, a call to sqliteInitDriver, but returns a DataStoreDriver instead a SQLiteDriver object.
#'
#' @param max.con     default 200L
#' @param fetch.default.rec     default 500
#' @param force.reload     default FALSE
#' @param shared.cache     default FALSE
data_store_driver <- function (max.con = 200L, fetch.default.rec = 500, force.reload = FALSE, shared.cache = FALSE) 
    .DataStoreDriver(RSQLite::sqliteInitDriver(max.con, fetch.default.rec, force.reload, shared.cache))


#' dbConnect method that creates a SQLiteDataStore
#'
#' The method is almost identical with its ancestor.
#' The only difference is that is returns a
#' SQLiteDataStore object instead of a SQLiteConnection. 
#'
#' @param drv    an DataStoreDriver object
#'
#' @docType methods
#' @name dbConnect
#' @rdname dbConnect-methods
#' @aliases dbConnect,DataStoreDriver-method
#' @export
setMethod(
  f="dbConnect",
  signature=c(drv="DataStoreDriver"),
  definition=function (drv, ...) {
    obj <- callNextMethod()
    new("SQLiteDataStore", Id=obj@Id)
  }
)

#' @rdname dbConnect-methods
#' @aliases dbConnect,SQLiteDataStore-method
setMethod(
  f="dbConnect",
  signature=c(drv="SQLiteDataStore"),
  definition=function (drv, ...) {
    obj <- callNextMethod()
    new("SQLiteDataStore", Id=obj@Id)
  }
)

#' datastore -- Connect to local database
#'
#' Use this function to connect to a local sqlite database.
#' It is essentially a call to \code{dbConnect}.
#'
#' @param fn   file name of the sqlite database. Default :memory:
#' @export
datastore <- function(fn=":memory:") dbConnect(data_store_driver(), fn)

#' dbCreateStmt -- create a SQL DDL statement with UNIQUE constraints
#'
#' Internal function. Builds SQL DDL statement. Code taken from RSQLite package
#' and slightly adapted.
#'
#' @param dbObj  DBIConnection object
#' @param name   table name
#' @param value  data.frame to store
#' @param uniq   column names for which to build a UNIQUE constraint, default=NULL
#' @param field.types use this types. default NULL
#'
dbCreateStmt <- function (dbObj, name, value, uniq=NULL, field.types = NULL) {
    if (!is.data.frame(value)) value <- as.data.frame(value)
    for(u in uniq) if(!u %in% colnames(value)) stop("not a possible unique column name '", u, "'.")
    if (is.null(field.types)) field.types <- sapply(value, dbDataType, dbObj = dbObj)
    names(field.types) <- make.db.names(dbObj, names(field.types), allow.keywords = FALSE)
    flds <- paste(names(field.types), field.types)
    stmt <- "CREATE TABLE %s (\n\t%s"
    if(length(uniq) >0) {
      stmt <- paste(stmt, ",\n\tUNIQUE (%s)\n)\n", sep="") 
      unis <- make.db.names(dbObj, uniq, allow.keywords=FALSE)
      stmt <- sprintf(stmt, name, paste(flds, collapse = ",\n\t"), paste(unis, collapse = ",\n\t"))
    } else {
      stmt <- paste(stmt, "\n)\n", sep="") 
      stmt <- sprintf(stmt, name, paste(flds, collapse = ",\n\t"))
    }
    return(stmt)
}

#' dbWriteTable method with update and conversion options.
#'
#' The method is almost identical with its ancestor.
#' The only difference is that supports updates instead of 
#' appends/replacements. It also provides a conversion mechanism 
#' for temporal attributes.
#'
#' @param conn    an SQLiteDataStore object
#' @param name    the name of the data table to create
#' @param value   the data object
#' @param savemode optional write mode "u" for update (default), "w" for replacement of the table
#' @param uniq    optional - column names that must be unique combinations. Used in "u" savemode.
#' @param dates   optional - column names that are to interpreted as dates. Converts them back and forth.
#' @param timestamps optional - column names that are intepreted as timestamps. Converts them back and forth.
#' @param field.types optional - class list of fields. See RSQLite package.
#' @param verbose optional - print diagnostic messages
#'
#' @docType methods
#' @name dbWriteTable
#' @export
#' @rdname dbWriteTable-methods
#' @aliases dbWriteTable,SQLiteDataStore,character,data.frame-method
setMethod(
  f="dbWriteTable",
  signature=c(conn="SQLiteDataStore", name="character", value="data.frame"),
  definition=function (conn, name, value, saveMode="u", uniq=c(), dates=c(), timestamps=c(), field.types = NULL, verbose=getOption("verbose"), ...) {
    if(verbose) cat("Start\n")
    if (length(dbListResults(conn))) {
        if(verbose) cat("creating connection\n")
        new.con <- dbConnect(conn)
        on.exit(dbDisconnect(new.con))
    } else {
        new.con <- conn
    }
    foundTable <- dbExistsTable(conn, name)
    new.table <- !foundTable
    createTable <- (new.table || foundTable && saveMode=="w")
    removeTable <- (foundTable && saveMode=="w")
    success <- dbBeginTransaction(conn)
    if (!success) {
        warning("unable to begin transaction")
        return(FALSE)
    }
    if (removeTable) {
        if(verbose) cat("removing table '", name,"'\n")
        success <- tryCatch({
            if (dbRemoveTable(conn, name)) {
                TRUE
            }
            else {
                warning(paste("table", name, "couldn't be overwritten"))
                FALSE
            }
        }, error = function(e) {
            warning(conditionMessage(e))
            FALSE
        })
    }
    if (!success) {
        dbRollback(conn)
        return(FALSE)
    }
    if (createTable) {
        if(verbose) cat("creating table '", name,"'\n")
        sql <- dbCreateStmt(new.con, name, value, field.types = field.types, uniq=uniq)
        success <- tryCatch({
            dbGetQuery(new.con, sql)
            TRUE
        }, error = function(e) {
            warning(conditionMessage(e))
            FALSE
        })
        if (!success) {
            dbRollback(conn)
            return(FALSE)
        }
    }
    if(length(dates)>0) {
      if(verbose) cat("converting date columns '", paste(dates, collapse=", "), "'\n")
      for (d in dates) {
        if(!(d %in% colnames(value))) warning("ignoring invalid date column '", d, "'.")
        value[,d] <- strftime(value[,d], "%Y-%m-%d")
      }
    }
    if(length(timestamps)>0) {
      if(verbose) cat("converting timestamp columns '", paste(dates, collapse=", "), "'\n")
      for (d in timestamps) {
        if(!(d %in% colnames(value))) warning("ignoring invalid date column '", d, "'.")
        value[,d] <- as.numeric(value[,d])
      }
    }    
    
    if(verbose) cat("writing data into table '", name,"'\n")
    valStr <- paste(rep("?", ncol(value)), collapse = ",")
    sql <- sprintf("insert or replace into %s values (%s)", name, valStr)
    success <- tryCatch({
        ret <- FALSE
        rs <- dbSendPreparedQuery(new.con, sql, bind.data = value)
        ret <- TRUE
    }, error = function(e) {
        warning(conditionMessage(e))
        ret <- FALSE
    }, finally = {
        if (exists("rs")) 
            dbClearResult(rs)
        ret
    })
    if (!success) 
        dbRollback(conn)
    else {
        success <- dbCommit(conn)
        if (!success) {
            warning(dbGetException(conn)[["errorMsg"]])
            dbRollback(conn)
        }
    }
    success
  }
)


#' dbReadTable method with update and conversion options.
#'
#' Counterpart to dbWriteTable. Ensures conversion of dates and timestamps.
#'
#' @param conn    an SQLiteDataStore object
#' @param name    the name of the data table to create
#' @param value   the data object
#' @param dates   optional - column names that are to interpreted as dates. Converts them back and forth.
#' @param timestamps optional - column names that are intepreted as timestamps. Converts them back and forth.
#' @param verbose optional - print diagnostic messages
#' @param ...     additional arguments, passed to ancestor
#'
#' @docType methods
#' @name dbReadTable
#' @export
#' @rdname dbReadTable-methods
#' @aliases dbReadTable,SQLiteDataStore,character-method
#' @aliases dbReadTable,SQLiteDataStore,character,ANY-method
setMethod(
  f="dbReadTable",
  signature=c(conn="SQLiteDataStore", name="character"),
  definition=function (conn, name, dates=c(), timestamps=c(), verbose=getOption("verbose"), ...) {
    if(verbose) cat("calling inherited method\n")
    value <- callNextMethod(conn, name, ...)
    if(length(dates)>0) {
      if(verbose) cat("converting date columns '", paste(dates, collapse=", "), "'\n")
      for (d in dates) {
        if(!(d %in% colnames(value))) warning("ignoring invalid date column '", d, "'.")
        value[[d]] <- strptime(value[,d], "%Y-%m-%d")
      }
    }
    if(length(timestamps)>0) {
      if(verbose) cat("converting timestamp columns '", paste(dates, collapse=", "), "'\n")
      for (d in timestamps) {
        if(!(d %in% colnames(value))) warning("ignoring invalid date column '", d, "'.")
        value[,d] <- as.POSIXct(value[,d], origin="1970-01-01")
      }
    }
    return(value)
  }
)
