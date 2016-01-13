
ownership_metrics_project = function(contribs) {
  own_m = do.call(rbind, lapply(unique(contribs$module), function(mod) {
    ownership_metrics(subset(contribs, module == mod), contribs, mod)
  }))
  merge(own_m, maf_metric(contribs))
}

ownership_metrics = function(contribs_mod, contribs_proj, module, own_threshold = 0.05) {

  total_churn = sum(contribs_mod$churn)

  devs = function(x) {if (nrow(x) == 0) character(0) else unique(x$developer)}
  ndev = function(x) {length(devs(x))}

  res = data.frame(module=module, project=contribs_proj$project[1],
                   NumDev = ndev(contribs_mod),
                   Churn = total_churn,
                   MVO = max(contribs_mod$churn/total_churn),
                   Major = nrow(subset(contribs_mod, churn/total_churn >= own_threshold)),
                   Minor = nrow(subset(contribs_mod, churn/total_churn < own_threshold))
  )
  res[is.nan(res)] = 0

  res
}


maf_metric = function(contribs_proj) {
  library(bipartite)
  touches_matrix = dcast(droplevels(contribs_proj), module ~ developer, value.var ="churn", fun.aggregate = sum, fill=0)
  row.names(touches_matrix) = touches_matrix$module
  touches_matrix$module = NULL
  touches_matrix = as.matrix(touches_matrix)
  res = specieslevel(touches_matrix, index="d", level = "lower")
  res$module = row.names(res)
  res$MAF = res$d
  res$d = NULL
  res
}
