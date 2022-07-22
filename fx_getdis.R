#' fx_getdis Function.
#'
#' Description.
#'
#' @description
#' Description.
#'
#' @param data.frame #do.
#'
#' @return vector #vector
#'
#' @examples
#' #fx_getdis(do)
#'
#' @export

fx_getdis <- function(do){
  vector1 = vector()
  vector2 = vector()
  x = length(grep("sub|typ|sign|clean|val|ref", c(names(do)))) * -1
  for(i in 1:nrow(do)) {
    vector1 = c(do[i, -1:x], recursive = TRUE, use.names = FALSE)
    vector2 = c(vector2, vector1[!is.na(vector1)])
  }

  vector2 = unique(vector2)

  #LIMPIA SOLO VALORES QUE CONTENGAN "=!><"
  enc = grep("[=!><]", vector2)
  for(i in 1:length(enc)) {
    final = (regexpr("[=!><]", vector2[enc[i]])[1]) - 1
    vector2[enc[i]] = substring(vector2[enc[i]], 1, final)
  }

  vector2 = gsub(" ", "", vector2)
  vector2 = gsub("strata", "", vector2)
  vector2 = gsub("[()]", "", vector2)
  vector2 = unique(vector2)

  return(vector2)
}

