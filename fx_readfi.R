#' fx_readfi Function.
#'
#' Description.
#'
#' @description
#' Description.
#'
#' @param character #ext.
#' @param boolean   #nam.
#' @param boolean   #log.
#' @export
#'
#' @return data.frame #db
#'
#' @examples
#' #fx_execox("csv", F, F)
#'
###############################################################################################################################
fx_readfi = function(ext = "csv", nam = F, log = F) {
  if (ext == "csv") {
    l_re = read.csv(file.path(l_x <- choose.files(default = "", caption = "Csv File", multi = FALSE, filters = c("csv files (*.csv)","*.csv"), index = 1)), header = TRUE, stringsAsFactors = FALSE, na.strings = "")
  }

  if (ext == "dta") {
    l_re = read_dta(file.path(l_x <- choose.files(default = "", caption = "Dta File", multi = FALSE, filters = c("dta files (*.dta)","*.dta"), index = 1)))
  }

  if (nam == T) {
    l_text = substring(l_x, max(gregexpr("[\\]", l_x)[[1]]) + 1, nchar(l_x))
    l_varnam = substring(l_text, 1, regexpr("[.]", l_text) - 1)
    assign("g_varnam", l_varnam, envir = .GlobalEnv)
  }

  if (log == T) {
    l_text = substring(l_x, max(gregexpr("[\\]", l_x)[[1]]) + 1, nchar(l_x))
    l_varnam = substring(l_text, 1, regexpr("[.]", l_text) - 1)
    l_varlog = paste(l_varnam, format(Sys.time(), "%m%d%H%M.txt"), sep = "_")
    assign("g_varlog", l_varlog, envir = .GlobalEnv)
  }
  return(l_re)
}
