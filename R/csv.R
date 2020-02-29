#' Read or Write CSV Using Selected Conventions
#' 
#' Reads or writes CSV files in a conventional way.
#' Generic, with methods for character and data.frame.
#' A length-one character argument is treated as a
#' filepath and tries to return a data.frame.
#' A data.frame argument is written to the specified
#' filepath. Typically, \code{quote} and \code{row.names}
#' are FALSE and \code{na} is ".".  When reading, white
#' space and empty strings are treated as NA, and \code{strip.white}
#' is TRUE. When writing, values with commas or
#' double-quotes are double-quoted (and embedded double-quotes are doubled).
#'  
#' @param x object
#' @param ... passed arguments
#' @seealso \code{\link{as.csv.character}}, \code{\link{as.csv.data.frame}}
#' @family as.csv
#' @export
#' @examples
#' data <- head(Theoph)
#' filepath <- file.path(tempdir(),'theoph.csv')
#' as.csv(data,filepath)
#' as.csv(filepath)

as.csv <- function(x,...)UseMethod('as.csv')

#' Treat Character as CSV filename.
#' 
#' Treat a character string as a CSV filename.
#' 
#' If x is character, is length one, and is a path to a file, an attempt is made to read the file.  
#' @param x character file path
#' @param as.is passed to \code{\link{read.csv}}
#' @param na.strings passed to \code{\link{read.csv}}
#' @param strip.white passed to \code{\link{read.csv}}
#' @param check.names passed to \code{\link{read.csv}}
#' @param source whether to assign x as the source attribute of the return value
#' @param ... passed to \code{\link{read.csv}} if accepted by \code{\link{read.table}}
#' @return data.frame, with attribute 'source' set to x
#' @family as.csv
#' @export
as.csv.character <- function(
  x,
  as.is=TRUE,
  na.strings=c('','\\s','.','NA'),
  strip.white=TRUE,
  check.names=FALSE,
  source = getOption('csv_source', TRUE),
  ...
){
  stopifnot(is.logical(source))
  stopifnot(length(source) == 1)
  stopifnot(length(x)==1)
  stopifnot(file.exists(x))
  args <- list(...)
  form <- names(formals(utils::read.csv))
  args <- args[names(args) %in% form]
  y <- do.call(
    utils::read.csv,
    c(
      list(
        x,
        as.is=as.is,
        na.strings=na.strings,
        strip.white=strip.white,
        check.names=check.names
      ),
      args
    )
  )
  if(source) attr(y,'source') <- x
  y
}

#' Save a Data Frame as CSV.
#' 
#' Saves a data.frame as CSV, using selected conventions.
#' 
#' @param x data.frame
#' @param file passed to \code{\link{write.csv}}
#' @param na passed to \code{\link{write.csv}}
#' @param quote passed to \code{\link{write.csv}}
#' @param auto double-quote column names and row values with embedded commas or double-quotes; the latter are escaped by doubling them
#' @param row.names passed to \code{\link{write.csv}}
#' @param ... passed to \code{\link{write.csv}} if accepted by \code{\link{write.table}}
#' @return invisible data.frame (x)
#' @export
#' @family as.csv
#' @examples 
#' x <- data.frame(
#'   check.names = FALSE,
#'   person = 1:3, 
#'   `name, suffix` = c("Bill Smith", 'Joseph "Joe" Hancock', "Mary Laguire, DDS")
#' )
#' file <- tempfile()
#' as.csv(x,file)
#' y <- as.csv(file,as.is=FALSE)
#' attr(y,'source')
#' attr(y,'source') <- NULL
#' x
#' y
#' stopifnot(identical(x,y))

as.csv.data.frame <- function(x, file, na='.', quote=FALSE, auto=!quote, row.names=FALSE, ...){
  comma <- sapply(x,function(col) any(grepl(',',col)))
  nms <- names(comma)[comma]
  if(length(nms) & !quote & !auto)warning(
    'quote and auto are false but found comma(s) in ',
    paste(nms,collapse=', ')
  )
  dup <- x[duplicated(x),,drop = FALSE]
  if(nrow(dup))warning(
    'found duplicate(s) e.g.:\n',
    paste(t(dup[1,]),collapse=', ')
  )
  if(auto)x[] <- lapply(x, autoquote)
  if(auto)names(x) <- autoquote(names(x))
  args <- list(...)
  form <- names(formals(utils::write.csv))
  args <- args[names(args) %in% form]
  do.call(
    utils::write.csv,
    c(
      list(x,file=file,na=na,quote=quote,row.names=row.names),
      args
    )
  )
  invisible(x)
}

.autoquote <- function(x,...){
  hasComma <- grepl(',',x)
  hasQuote <- grepl('"',x)
  mustQuote <- hasComma | hasQuote
  x[hasQuote] <- gsub('"','""', x[hasQuote])
  x[mustQuote] <- paste0('"',x[mustQuote],'"')
  x
}

#' Autoquote
#' 
#' Autoquote.  Generic, with default, character, and factor methods.
#' @family autoquote
#' @export
#' @keywords internal
#' @param x object
#' @param ... passed arguments
autoquote <- function(x,...)UseMethod('autoquote')

#' Autoquote Default
#' 
#' Autoquote default.  Returns its argument.
#' @family autoquote
#' @export
#' @keywords internal
#' @param x object
#' @param ... passed arguments
autoquote.default <- function(x,...)x


#' Autoquote Character
#' 
#' Autoquote character.  Quotes elements with commas or double quotes; the latter are doubled.
#' @family autoquote
#' @export
#' @keywords internal
#' @param x object
#' @param ... passed arguments
autoquote.character <- function(x,...).autoquote(x,...)


#' Autoquote Factor
#' 
#' Autoquote character.  Quotes levels with commas or double quotes; the latter are doubled.
#' @family autoquote
#' @export
#' @keywords internal
#' @param x object
#' @param ... passed arguments
autoquote.factor <- function(x,...){
  levels(x) <- autoquote(levels(x))
  x
}




