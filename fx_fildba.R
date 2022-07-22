#' fx_fildba Function.
#'
#' Description.
#'
#' @description
#' Description.
#'
#' @param data.frame #db.
#' @param vector #quvector.
#'
#' @return data.frame #db
#'
#' @examples
#' #fx_fildba(db, vector)
#'
#' @export

fx_fildba = function(f_db, f_quvector){
  l_part = paste(paste("subset(f_db, ", paste(f_quvector, collapse = " & ")), ")", sep = "")
  l_exp = parse(text = l_part)
  f_db = eval(l_exp)
  return(f_db)
}

