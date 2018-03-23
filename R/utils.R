########################################################################################################################

## parameter.is.flag
##
## Checks if the provided parameter value is a flag.
## 
## @param value Value to be tested.
## @return \code{TRUE} if \code{values} is \code{TRUE} or \code{FALSE}, \code{FALSE} otherwise.
## @author Yassen Assenov
parameter.is.flag <- function(value) {
	is.logical(value) && length(value) == 1 && (!is.na(value))
}

########################################################################################################################

## Validates the given vector or list contains a single element. This function is used in validating function or method
## arguments.
##
## @param x          Value vector or list to validate.
## @param param.name Name of parameter or slot that is validated. This is used in the generation of failing message.
## @return Short message that encodes the result of the validation in the form of a \code{character}. It is either the
##         string \code{ok}, or a short phrase describing the divergence from the "single value assumption".
## @author Yassen Assenov
validate.single <- function(x, param.name = "x") {
	if (is.null(x) || length(x) == 0) {
		result <- paste("missing value for", param.name)
	} else if (length(x) > 1) {
		result <- paste("multiple values for", param.name)
	} else if (is.na(x)) {
		result <- paste("missing value for", param.name)
	} else {
		result <- "ok"
	}
	return(result)
}


########################################################################################################################

#' validate.file
#'
#' Validates the specified file or directory exists. Prints an error or a warning message to the log if it does not
#' exist, it is not of the accepted type or is not accessible.
#'
#' @param file      Name of file or directory to validate.
#' @param is.file   Flag indicating if the given name must denote an existing file. If this is \code{FALSE}, the given
#'                  name must denote a directory. Set this to \code{NA} if both types are an acceptable scenario.
#' @param terminate Flag indicating if the execution is to be terminated in case the validation fails. This parameter
#'                  determines if an error message (\code{terminate} is \code{TRUE}) or a warning message
#'                  (\code{terminate} is \code{FALSE}) is to be sent to the log when the specified file or directory
#'                  does not exist, is not of the accepted type or is not accessible.
#' @return Whether the validation succeeded or not, invisibly. Note that when \code{terminate} is \code{TRUE} and the
#'         validation fails, the R session is closed and thus no value is returned.
#'
#' @examples
#' \donttest{
#' # Validate the current working directory exists
#' validate.file(getwd(), FALSE)
#' }
#' @author Yassen Assenov
#' @noRd
validate.file <- function(file, is.file = TRUE, terminate = TRUE) {
	if (!(is.character(file) && length(file) == 1 && (!is.na(file)))) {
		stop("invalid value for file; expected single character")
	}
	if (!parameter.is.flag(is.file)) {
		stop("invalid value for is.file; expected TRUE or FALSE")
	}
	if (!parameter.is.flag(terminate)) {
		stop("invalid value for terminate; expected TRUE or FALSE")
	}
	file <- file[1]
	is.file <- is.file[1]
	terminate <- terminate[1]
	if (!file.exists(file)) {
		msg <- ifelse(is.na(is.file), "File / directory", ifelse(is.file, "File", "Directory"))
		msg <- c(msg, "not found:", file)
		if (terminate) {
			stop(msg)
		}
		warning(msg)
		return(invisible(FALSE))
	}
	if (!is.na(is.file)) {
		is.dir <- file.info(file)[1, "isdir"]
		if (is.file == is.dir) {
			msg <- c(file, "is a", ifelse(is.dir, "directory", "file"))
			if (terminate) {
				stop(msg)
			}
			warning(msg)
			return(invisible(FALSE))
		}
	}
	return(invisible(TRUE))
}

########################################################################################################################

## If there is a logger initialized, validates that the given directory exists.
##
## @param dname Name of directory to be validated.
## @author Yassen Assenov
validate.dir <- function(dname) {
	validate.file(dname, is.file = FALSE)
}

########################################################################################################################

## write.line
##
## Writes a line of text to the specified text file. This function is used in the generation of HTML reports.
##
## @param txt    Character vector storing the text to be written. The elements of this vector are concatenated without
##               a separator.
## @param fname  Name of the file to write the text to.
## @param indent Indentation of the text, given as number of \code{TAB} characters.
## @param append Flag indicating if the line is to be appended to the text file. If this is \code{FALSE}, the file's
##               contents are overwritten.
## @author Yassen Assenov
write.line <- function(txt, fname, indent = 0, append = TRUE) {
	strprefix <- paste(rep("\t", times = indent), collapse = "")
	cat(strprefix, paste0(txt, collapse = ""), "\n", file = fname, sep = "", append = append)
}

########################################################################################################################

#' ggMsgPlot
#'
#' Creates a plot, using \pkg{ggplot2}, with a single text message.
#'
#' @param txt Text to be plotted.
#' @return The newly initialized \code{ggplot} instance.
#'
#' @examples
#' \donttest{
#' x11(width = 5, height = 5)
#' ggMsgPlot("Missing data")
#' }
#' @author Yassen Assenov
#' @export
ggMsgPlot <- function(txt) {
	if (!(is.character(txt) && length(txt) == 1 && (!is.na(txt)))) {
		stop("invalid value for txt")
	}
	ggplot(data.frame(x = 1, y = 1, labeltext = txt), aes_string("x", "y", label = "labeltext")) +
		geom_text(color = "grey50") +
		theme(axis.line = element_blank(), axis.title = element_blank(), axis.text = element_blank(),
			axis.ticks = element_blank(), panel.border = element_blank(), panel.grid = element_blank(),
			panel.background = element_blank(), plot.background = element_blank())
}
