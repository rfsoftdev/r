#' fx_execox Function.
#'
#' Description.
#'
#' @description
#' Description.
#'
#' @param data.frame #db.
#' @param data.frame #do.
#' @param character  #log.
#' @export
#'
#' @return vector #vector
#'
#' @examples
#' #fx_execox(db, do, log)
#'
###############################################################################################################################
fx_execox = function(f_db, f_do, f_varlog){
  resvector = vector()
  id = ""
  flog.appender(appender.file(f_varlog), name = f_varlog)
  for(i in 1:nrow(f_do)) {
    quvector = c(f_do[i, grep("query", c(names(f_do)))], recursive = TRUE, use.names = FALSE)
    quvector = quvector[!is.na(quvector)]
    idnew = paste(quvector, collapse = "")
    if(id != idnew) {
      dbf = fx_fildba(f_db, quvector)
      id = idnew
    }
    etvector = c(f_do[i, grep("typ|sign|clean|sub", c(names(f_do)))], recursive = TRUE, use.names = TRUE)
    etvector[is.na(etvector)] = ""
    tivector = c(f_do[i, grep("surv", c(names(f_do)))], recursive = TRUE, use.names = FALSE)
    tivector = trimws(tivector[!is.na(tivector)])
    expvar = trimws(f_do[i, grep("exp", c(names(f_do)))])
    covector = c(f_do[i, grep("co", c(names(f_do)))], recursive = TRUE, use.names = FALSE)
    covector = trimws(covector[!is.na(covector)])
    m = fx_execut(dbf, etvector, tivector, expvar, covector, quvector, f_varlog)
    resvector = rbind(resvector, m)
    print(paste(paste("LINE: ", i+1, sep = ""), paste(etvector, collapse = " - "), sep = " - "))
  }
  return(resvector)
}
fx_setime = function(f_db, f_tivector, clean = T) {
  if(length(f_tivector) == 2) {
    f_db$surv = Surv(as.numeric(f_db[[f_tivector[1]]]), f_db[[f_tivector[2]]])
  }
  if(length(f_tivector) == 3) {
    f_db$surv = Surv(as.numeric(f_db[[f_tivector[1]]] - f_db[[f_tivector[2]]]), f_db[[f_tivector[3]]])
  }
  if(length(f_tivector) == 4) {
    f_db$surv = Surv(pmin(as.numeric(f_db[[f_tivector[4]]] - f_db[[f_tivector[2]]]), as.numeric(f_db[[f_tivector[1]]] - f_db[[f_tivector[2]]])), f_db[[f_tivector[3]]])
  }
  if(clean) {
    f_db = f_db[-which(f_db$surv[, 1] < 0 & f_db$surv[, 2] == 1), ]
  }
  return(f_db)
}
fx_execut = function(f_db, f_etvector, f_tivector, f_exposure, f_covector, f_quvector, f_logvar){

  l_revector = vector()
  l_type = f_etvector[1]
  l_sigvar = f_etvector[2]
  l_clean = f_etvector[3]
  l_ncat = 0

  #f_db$surv = Surv(as.numeric(f_db[[f_tivector[1]]] - f_db[[f_tivector[2]]]), f_db[[f_tivector[3]]])
  #f_db = f_db[-which(f_db$surv[, 1] < 0 & f_db$surv[, 2] == 1), ]

  f_db = fx_setime(f_db, f_tivector, l_clean)

  if (nrow(f_db) >= 1) {

    if(l_type == "i"){
      f_db = fx_setfac(T, f_db, f_exposure)
      l_ncat = length(levels(f_db[[f_exposure]])) - 1
    }

    l_cox = fx_getcox(f_db, f_covector, f_exposure, l_sigvar)
    l_sum = summary(l_cox)

    flog.info(paste(rep("#", time = 250), collapse = ""), list("DETAILS" = fx_getlog(f_etvector, f_tivector, f_exposure, f_covector, f_quvector), "SUMMARY" = l_sum), capture = TRUE, name = f_logvar)

    f_db$example <- !seq_len(nrow(f_db))%in%na.action(l_cox);
    f_db = subset(f_db, example == TRUE)

    if(l_type == "i"){
      l_db = subset(f_db, f_db[[f_tivector[3]]] == 1)
      devector = summary(l_db[[f_exposure]])
      levector = levels(f_db[[f_exposure]])
      tovector = summary(f_db[[f_exposure]])
      l_revector = fx_getres(l_sum, f_etvector, l_ncat, f_exposure, levector, tovector, devector)
    } else {
      if (l_sigvar == "*") {

        vec = c(1, 2, length(l_sum[["conf.int"]][, 1]))

        l_etvector = vector()
        for(i in 1:length(vec)) {
          l_etvector = rbind(l_etvector, f_etvector)
        }

        l_revector =
          cbind(
            l_etvector,
            "exposure" = row.names(l_sum[["conf.int"]])[vec],
            "Total" = l_sum[["n"]],
            "event/deaths" = l_sum[["nevent"]],
            "HR" = l_sum[["conf.int"]][vec, 1],
            l_sum[["conf.int"]][vec, -1:-2],
            "P value" = l_sum[["coefficients"]][vec, 5],
            "exp(-coef)" = l_sum[["conf.int"]][vec, 2],
            l_sum[["coefficients"]][vec, 1:4])

      } else {
        l_revector =
          c(
            f_etvector,
            "exposure" = f_exposure,
            "Total" = l_sum[["n"]],
            "event/deaths" = l_sum[["nevent"]],
            "HR" = l_sum[["conf.int"]][1, 1],
            l_sum[["conf.int"]][1, -1:-2],
            "P value" = l_sum[["coefficients"]][1, 5],
            "exp(-coef)" = l_sum[["conf.int"]][1, 2],
            l_sum[["coefficients"]][1, 1:4],
          recursive = FALSE)
      }
    }
  } else {
    l_revector = c(
      f_etvector,
      "exposure" = f_exposure,
      rep(1, time = 11))
  }
  return(l_revector)
}
fx_getlog = function(f_etvector, f_tivector, f_exposure, f_covector, f_quvector){
  l_etvector = paste(f_etvector, collapse = " - ")
  l_tivector = paste(f_tivector, collapse = " - ")
  l_exposure = f_exposure
  l_covector = paste(f_covector, collapse = " - ")
  l_quvector = paste(f_quvector, collapse = " - ")
  l_det = rbind("LABEL: " = l_etvector, "TIME: " = l_tivector, "EXPOSURE: " = l_exposure, "COVARIANTS: " = l_covector, "QUERY: " = l_quvector)
  colnames(l_det) = "DO CONTENT"
  return(l_det)
}
fx_getcox = function(f_df, f_covector, f_exposure, f_sigvar = "+", f_pspline = F){
  if (f_pspline) {
    f_exposure = paste(paste("pspline(", f_exposure, sep = ""), ", k = 3)", sep = "")
  }
  if (length(f_covector) == 0) {
    f_sigvar = ""
  }
  part1 = paste(paste("coxph(surv", f_exposure, sep = " ~ "), f_sigvar, sep = " ")
  part2 = paste(part1, paste(f_covector, collapse = " + "), sep = " ")
  part3 = paste(part2, ", data = f_df)", sep = "")
  myexp = parse(text = part3)
  #print(myexp)
  l_cox = eval(myexp)
  return(l_cox)
}
fx_getres = function(f_su, f_etvector, f_ncat, f_exposure, f_levector, f_tovector, f_devector){
  l_etvector = vector()
  l_result = vector()
  for(i in 1:f_ncat) {
    l_etvector = rbind(l_etvector, f_etvector)
  }
  x = f_ncat + 1

  for(i in 1:x) {
    f_levector[i] = paste(f_exposure, f_levector[i], sep = " - ")
  }

  l_result = cbind(
    l_etvector,
    exposure = f_levector[-1],
    "total" = f_tovector[-1], #"total" = rep(f_su[["n"]], f_ncat),
    "event/deaths" = f_devector[-1], #"event/deaths" = rep(f_su[["nevent"]], f_ncat),
    "HR" = f_su[["conf.int"]][1:f_ncat, 1],
    f_su[["conf.int"]][1:f_ncat, -1:-2],
    "p value" = f_su[["coefficients"]][1:f_ncat, 5],
    "exp(-coef)" = f_su[["conf.int"]][1:f_ncat, 2],
    f_su[["coefficients"]][1:f_ncat, 1:4])

  ones = c(f_tovector[1], f_devector[1], rep(1, time = 9))
  names(ones) =
    c(
      "total",
      "event/deaths",
      "HR",
      "lower .95",
      "upper .95",
      "p value",
      "exp(-coef)",
      "coef",
      "exp(coef)",
      "se(coef)",
      "z")

  l_result =
    rbind(
      c(f_etvector, "exposure" = f_levector[1], ones),
      l_result)

  return(l_result)
}
fx_setfac = function(f_isfac, f_db, f_column, f_label = NA, f_units = NA){
  if(f_isfac) {
    f_db[[f_column]] = as_factor(f_db[[f_column]], levels = "both")
    l_len = length(levels(f_db[[f_column]]))
    for(i in 1:l_len) {
      valor = levels(f_db[[f_column]])[i]
      levels(f_db[[f_column]])[i] = trimws(substring(valor, regexpr("[.]", valor)[1] + 1, nchar(valor)))
    }
  }
  if(!is.na(f_label)) {
    label(f_db[[f_column]]) = f_label
  }
  if(!is.na(f_units)) {
    units(f_db[[f_column]]) = f_units
  }
  return(f_db)
}
