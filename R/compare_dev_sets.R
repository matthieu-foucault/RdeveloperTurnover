
devs = function(x) {
  if (nrow(x) == 0) return(character(0))
  unique(x$developer)
}

dice_devs = function(contribs_a,contribs_b) {
  a = devs(contribs_a)
  b = devs(contribs_b)
  if ((length(a) + length(b)) == 0) return(1)
  2 * length(intersect(a,b)) / (length(a) + length(b))
}


compare_dev_sets = function(period_month_size) {
  lapply(ls(dt_env$projects_names), function(name_p) {
    contribs = get_contributions(name_p, period_month_size)

    dm_allp_bef = rbind(contribs$before, subset(dt_env$act_months_before, project == name_p & releaseDate < min(contribs$before$releaseDate)))
    dm_allp_aft = rbind(contribs$after, subset(dt_env$act_months_after, project == name_p))

    dices = do.call(rbind, lapply(unique(c(contribs$before$module, contribs$after$module)), function(mod) {
      dev_mod_before_1p = subset(contribs$before, module == mod)
      dev_mod_after_1p = subset(contribs$after, module == mod)

      dev_mod_before_all = subset(dm_allp_bef, module == mod)
      dev_mod_after_all = subset(dm_allp_aft, module == mod)

      developer_sets_limited = developer_sets(dev_mod_before_1p, dev_mod_after_1p, contribs$before, contribs$after)
      developer_sets_newcomers_full = developer_sets(dev_mod_before_all, dev_mod_after_1p, dm_allp_bef, contribs$after)
      developer_sets_leavers_full = developer_sets(dev_mod_before_1p, dev_mod_after_all, contribs$before, dm_allp_aft)

      data.frame(IN = dice_devs(developer_sets_limited$IN, developer_sets_newcomers_full$IN),
                 IL = dice_devs(developer_sets_limited$IL, developer_sets_leavers_full$IL),
                 EN = dice_devs(developer_sets_limited$EN, developer_sets_newcomers_full$EN),
                 EL = dice_devs(developer_sets_limited$EL, developer_sets_leavers_full$EL))

    }))

    if (period_month_size == 0)
      main = paste0(dt_env$projects_names[[name_p]], ": |P| = 1 Release")
    else
      main = paste0(dt_env$projects_names[[name_p]], ": |P| = ", period_month_size, " Months")

    for (col in c("IN","IL","EN","EL")){
      if (sd(dices[,col]) == 0) {
        dices[1,col] = dices[1,col] + 0.00001
      }
    }
    m = suppressMessages(melt(dices))
    ggplot(m, aes(variable, value)) + geom_boxplot() + geom_violin(adjust=.5, scale="width",fill = "grey80") +
      theme_bw() + scale_y_continuous(limits = c(0, 1.1)) + labs(title=main, y="Dice coefficient")
  })

}


plot_dev_sets_comparison = function() {
  with(dt_env,{
    svg(paste0(web_working_dir, "/sets_similarity.svg"), height=20, width=20)
    multiplot(plotlist=c(compare_dev_sets(0),compare_dev_sets(1),compare_dev_sets(3),compare_dev_sets(6)),
               cols=5)
    dev.off()
  })
}

