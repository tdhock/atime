error_if_param_not_in_expr <- function(param.dt, elist){
  one.param.list <- as.list(param.dt[1])
  problem.list <- list()
  for(expr.name in names(elist)){
    before.sub <- elist[[expr.name]]
    for(param.name in names(one.param.list)){
      param.sub.list <- one.param.list[param.name]
      after.sub <- eval(substitute(
        substitute(EXPR, param.sub.list),
        list(EXPR=before.sub)))
      if(identical(paste(before.sub), paste(after.sub))){
        problem.list[[paste(expr.name, param.name)]] <- paste(
          param.name, "not in", expr.name)
      }
    }
  }
  if(length(problem.list)){
    stop(
      "each param should be present in each expr, problems: ",
      paste(problem.list, collapse=", "))
  }
}

atime_grid <- function
(param.list=list(),
  ...,
  name.value.sep="=",
  expr.param.sep=" ",
  collapse=",",
  symbol.params=character()
){
  if(!(is.character(symbol.params) && all(!is.na(symbol.params)))){
    stop("symbol.params must be a character vector with no missing values")
  }
  if(!(is.character(name.value.sep) && length(name.value.sep)==1 && !is.na(name.value.sep))){
    stop("name.value.sep must be a non-missing string")
  }
  if(!(is.character(expr.param.sep) && length(expr.param.sep)==1 && !is.na(expr.param.sep))){
    stop("expr.param.sep must be a non-missing string")
  }
  if(!is.list(param.list)){
    stop("param.list must be a named list of parameters")
  }
  if(any(names(param.list)=="")){
    stop("each element of param.list must be named")
  }
  formal.names <- names(formals())
  mc.args <- as.list(match.call()[-1])
  elist <- mc.args[!names(mc.args) %in% formal.names]
  if(is.null(names(elist)) || any(names(elist)=="")){
    stop("each expression in ... must be named")
  }
  if(length(param.list)==0)return(elist)
  CJ.arg.names <- setdiff(names(formals(CJ)),"...")
  bad.params <- intersect(CJ.arg.names,names(param.list))
  if(length(bad.params)){
    stop("param.list must not have elements named: ", paste(bad.params, collapse=", "))
  }
  bad.types <- !sapply(param.list, is.atomic)
  if(any(bad.types)){
    bad.names <- names(param.list)[bad.types]
    stop("param.list elements must be atomic, but some are not: ", paste(bad.names, collapse=", "))
  }
  param.dt <- do.call(CJ, param.list)
  error_if_param_not_in_expr(param.dt, elist)
  value.mat <- do.call(cbind, lapply(param.dt, paste))
  name.vec <- colnames(value.mat)[col(value.mat)]
  name.value.mat <- matrix(
    paste0(name.vec, name.value.sep, value.mat),
    nrow(value.mat), ncol(value.mat))
  name.value.vec <- apply(name.value.mat, 1, paste, collapse=collapse)
  out.list <- list()
  out.param.list <- list()
  for(expr.name in names(elist)){
    for(row.i in 1:nrow(param.dt)){
      param.name.value <- name.value.vec[[row.i]]
      out.name <- paste0(expr.name, expr.param.sep, param.name.value)
      param.row <- param.dt[row.i]
      param.row.list <- as.list(param.row)
      param.row.list[symbol.params] <- lapply(
        param.row.list[symbol.params], as.symbol)
      out.list[[out.name]] <- eval(substitute(
        substitute(EXPR, param.row.list), 
        list(EXPR=elist[[expr.name]])))
      out.param.list[[paste(expr.name, row.i)]] <- data.table(
        expr.name=out.name,
        expr.grid=expr.name,
        param.row)
    }
  }
  attr(out.list, "parameters") <- rbindlist(out.param.list)
  out.list
}
