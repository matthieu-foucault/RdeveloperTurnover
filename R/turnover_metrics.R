
## Returns the contributions for the P_1 and P_2 periods for a given project and period size
get_contributions = function(project_url, period_month_size = 0) {
  # 0 == release
  if (period_month_size == 0) {
    contribs = subset(dt_env$act_releases, project == project_url)
    releases = unique(contribs$releaseDate)
    contribs_before = subset(contribs, releaseDate == min(releases))
    contribs_after = subset(contribs, releaseDate == max(releases))
  } else {
    contribs_before = subset(dt_env$act_months_before, project == project_url & commits_group_id > period_month_size & commits_group_id <= period_month_size*2 )
    contribs_after = subset(dt_env$act_months_before, project == project_url & commits_group_id <= period_month_size) # the 1st period for leavers is the one before S0
  }
  list(before=contribs_before, after=contribs_after)
}

turnover_metrics_project = function(contribs) {
  do.call(rbind, lapply(unique(c(contribs$before$module, contribs$after$module)), function(mod) {
    contribs_mod_before = subset(contribs$before, module == mod)
    contribs_mod_after = subset(contribs$after, module == mod)
    turnover_metrics(contribs_mod_before, contribs_mod_after, contribs$before, contribs$after, mod)
  }))
}


is.nan.data.frame = function(x)
  do.call(cbind, lapply(x, is.nan))

developer_sets = function(contribs_mod_before, contribs_mod_after, contribs_proj_before, contribs_proj_after) {
  leavers = subset(contribs_mod_before,!(developer %in% contribs_mod_after$developer))
  newcomers = subset(contribs_mod_after,!(developer %in% contribs_mod_before$developer))

  ext_newcomers = subset(newcomers, !(developer %in% contribs_proj_before$developer))
  int_newcomers = subset(newcomers, developer %in% contribs_proj_before$developer)

  ext_leavers = subset(leavers, !(developer %in% contribs_proj_after$developer))
  int_leavers = subset(leavers, developer %in% contribs_proj_after$developer)

  stayers = merge(contribs_mod_after, contribs_mod_before,
                  by="developer", all=F, suffixes=c(".after",".before"))

  list(L = leavers,
       N = newcomers,
       EN = ext_newcomers,
       IN = int_newcomers,
       EL = ext_leavers,
       IL = int_leavers,
       S = stayers)
}

turnover_metrics = function(contribs_mod_before, contribs_mod_after, contribs_proj_before, contribs_proj_after, module) {

  sets = developer_sets(contribs_mod_before, contribs_mod_after, contribs_proj_before, contribs_proj_after)

  devs = function(x) {if (nrow(x) == 0) character(0) else unique(x$developer)}
  ndev = function(x) {length(devs(x))}

  ndev_before = ndev(contribs_mod_before)
  ndev_after = ndev(contribs_mod_after)

  res = data.frame(module=module, project=contribs_proj_before$project[1],
    NumDevsAfter = ndev_after,
    NumDevsBefore = ndev_before,
    NumDevsMean = mean(c(ndev_before,ndev_after)),
    EN = ndev(sets$EN),
    EL = ndev(sets$EL),
    IN = ndev(sets$IN),
    IL = ndev(sets$IL),
#     LR = ndev(sets$L)/ndev_before,
#     ILR = ndev(sets$IL)/ndev_before,
#     ELR = ndev(sets$EL)/ndev_before,
#     NR = ndev(sets$N)/ndev_after,
#     INR = ndev(sets$IN)/ndev_after,
#     ENR = ndev(sets$EN)/ndev_after,
#     LO = sum(sets$L$ownModuleChurn),
#     ILO = sum(sets$IL$ownModuleChurn),
#     ELO = sum(sets$EL$ownModuleChurn),
#     NO = sum(sets$N$ownModuleChurn),
#     INO = sum(sets$IN$ownModuleChurn),
#     ENO = sum(sets$EN$ownModuleChurn),
    S = ndev(sets$S),
#     SR = ndev(sets$S)/mean(c(ndev_before,ndev_after)),
#     SO = sum(rowMeans(sets$S[,c("ownModuleChurn.a","ownModuleChurn.b")])),
    ENA = sum(sets$EN$churn),
    ELA = sum(sets$EL$churn),
    INA = sum(sets$IN$churn),
    ILA = sum(sets$IL$churn),
    SA = sum(rowMeans(sets$S[,c("churn.after","churn.before")]))
  )

  res[is.nan(res)] = 0
  res$A = rowSums(res[,c("ENA", "INA","ILA","ELA", "SA")])


  res
}
