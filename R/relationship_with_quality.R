
cor.string = function(x, y) {
  library(boot)

  bootresult = boot(data.frame(x=x,y=y), function(df, idx) {with(df[idx,], cor.test(x,y, method="spearman")$estimate)}, R = 1000)
  ci = boot.ci(bootresult, type="bca", conf = 0.90)

  s = paste0("[", as.numeric(round(ci$bca[4],2)), " , ", as.numeric(round(ci$bca[5],2)) , "]")
}

spearman_tests = function(metrics, metrics_names, dep_variable="BugDensity") {
  do.call(rbind, by(metrics, metrics$project, function(metrics_p) {
    if (nrow(metrics_p) < 10) return()

    project_name = dt_env$projects_names[[metrics_p$project[1]]]

    do.call(rbind,lapply(metrics_names, function(metric_name){
      test = cor.test(metrics_p[,metric_name],metrics_p[,dep_variable], method="spearman")
      data.frame(project=project_name, metric=metric_name, spearman_estimate = test$estimate, spearman_pvalue = test$p.value )
    }))
  }))
}

spearman_bootstrap = function(metrics, metrics_names, dep_variable="BugDensity") {
  library(boot)

  do.call(rbind, by(metrics, metrics$project, function(metrics_p) {
    if (nrow(metrics_p) < 10) return()

    project_name = dt_env$projects_names[[metrics_p$project[1]]]

    do.call(rbind,lapply(metrics_names, function(metric_name){
      bootresult = boot(data.frame(x=metrics_p[,metric_name],y=metrics_p[,dep_variable]),
                        function(df, idx) {with(df[idx,], cor.test(x,y, method="spearman")$estimate)}, R = 1000)
      ci_90 = boot.ci(bootresult, type="bca", conf = 0.90)
      ci_95 = boot.ci(bootresult, type="bca", conf = 0.95)
      data.frame(project=project_name, metric=metric_name, lo_90 = ci_90$bca[4], hi_90 = ci_90$bca[5],
                 lo_95 = ci_95$bca[4], hi_95 = ci_95$bca[5])
    }))
  }))
}

compute_correlations = function() {
  compute_metrics = function(period_month_size) {
    do.call(rbind, lapply(ls(dt_env$projects_names), function(project_url) {
      contribs = get_contributions(project_url, period_month_size)
      turnover_metrics_project(contribs)
    }))
  }

  metrics_6m = merge(dt_env$mod_m, compute_metrics(6), by = c("module","project"), all=F)
  dt_env$metrics = metrics_6m

  mnames = c("INA","ILA","ENA","ELA","StA", "A")
  compute_cor = function(metr, html=F) {
    do.call(rbind, by(metr, metr$project, function(metrics_p) {
      if (nrow(metrics_p) < 10) return()
      c(dt_env$projects_names[[metrics_p$project[1]]],lapply(metrics_p[,mnames], function(x){
        cor.string(metrics_p$BugDensity, x)
        }))
    }))
  }
  corr_data = merge(spearman_tests(metrics_6m, mnames), spearman_bootstrap(metrics_6m, mnames))

  by(corr_data, corr_data$metric, function(corr_metric) {
    cairo_pdf(paste0(dt_env$working_dir,"/", "corr_", corr_metric$metric[1],".pdf"), width = 5, height = 5)
    print(
      ggplot(corr_metric, aes(project, spearman_estimate, ymin=lo_90, ymax=hi_90)) +
        geom_point(size=3, color="#0000CC") + geom_errorbar(width=0.25, color="#0000CC") + geom_abline(slope=0, intercept=0, lty=2) +
        geom_errorbar(data=corr_metric, aes(project, spearman_estimate, ymin=lo_95, ymax=hi_95), width=0.5, color="#0000CC") +
        ylim(c(-1,1)) + theme_bw() + labs(title=corr_metric$metric[1], x="Projet", y="Coefficient de correlation")
    )
    dev.off()
  })

  corr_6m = compute_cor(metrics_6m)
  write.table(corr_6m, file=paste0(dt_env$working_dir,"/bugscor.tex"), sep=" & ", quote=F, row.names=F, col.names=F, eol= " \\\\\n")
  dir.create(paste0(dt_env$web_working_dir,"/results"), showWarnings =F)
  write(htmlTable(compute_cor(metrics_6m, T), rnames = F,css.class="table"), paste0(dt_env$web_working_dir,"/results/corr_6m.html"))
  dir.create(paste0(dt_env$working_dir,"/patterns"), showWarnings =F)
  by(metrics_6m, metrics_6m$project, function(metrics_p) {
    name_p = dt_env$projects_names[[metrics_p$project[1]]]
    a = metrics_p[,c("A", "INA", "ENA", "ILA", "ELA", "StA")]
    rownames(a) = metrics_p$module
    a = subset(a[order(a$A),], A>0)
    colnames(a)[1] = "Total A"

    plot_patterns = function(row_labels = TRUE) {
      if (row_labels) {
        lab_row = NULL
        width = c(4,3)
        margins = c(6,1)
      }
      else {
        lab_row = ""
        width = c(4,0.1)
        margins = c(6,0.3)
      }
      heatmap.2(log(as.matrix(a) + 0.01), Rowv=NA, Colv=NA , col = colorRampPalette(brewer.pal(9,"Blues"))(100),
                margins = margins, cexCol = 2, rowsep=1:nrow(a), labRow = lab_row,
                sepcolor='#DEEBF7', sepwidth=c(0,0.00001),
                scale="none", main = name_p, density.info='none',
                trace='none',  key=FALSE, keysize=1.0, symkey=FALSE,
                lmat=rbind( c(3,2), c(1,4), c(0,0)), lhei=c(0.17,1.425, 0.05), lwid=width)
    }

    cairo_pdf(paste0(dt_env$working_dir,"/patterns/", name_p,"_fixes_metrics.pdf"), width = 8, height = 8)
    plot_patterns()
    dev.off()

    cairo_pdf(paste0(dt_env$working_dir,"/patterns/", name_p,"_fixes_metrics_nomodule.pdf"), width = 3, height = 8)
    plot_patterns(FALSE)
    dev.off()

    svg(paste0(dt_env$web_working_dir,"/results/", name_p,"_fixes_metrics.svg"), width = 8, height = 8)
    plot_patterns()
    dev.off()
  })

  # relative importance

  library(relaimpo)

  model = BugFixes ~ LoC + A + NumDevsMean + ENA + INA + StA
  by(metrics_6m, metrics_6m$project, function (x) {
    linmod = lm(model, data=x)
    #summary(linmod)
    relimp = calc.relimp(linmod, type ="pmvd")
    plot_pmvd(relimp, dt_env$projects_names[[x$project[1]]])
  })
}

compute_ownership_correlations = function() {
  compute_metrics = function() {
    do.call(rbind, lapply(ls(dt_env$projects_names), function(project_url) {
      period_month_size = dt_env$release_duration[[project_url]]
      contribs = subset(dt_env$act_months_before, project == project_url & commits_group_id <= period_month_size)
      ownership_metrics_project(contribs)
    }))
  }

  metrics_6m = merge(dt_env$mod_m, compute_metrics(), by = c("module","project"), all=F)
  dt_env$own_metrics = metrics_6m

  mnames = c("LoC","NumDev", "Churn", "MVO","Major","Minor", "MAF")

  corr_data = merge(spearman_tests(metrics_6m, mnames), spearman_bootstrap(metrics_6m, mnames))
  by(corr_data, corr_data$metric, function(corr_metric) {
    cairo_pdf(paste0(dt_env$working_dir,"/", "corr_", corr_metric$metric[1],".pdf"), width = 5, height = 5)
    print(
      ggplot(corr_metric, aes(project, spearman_estimate, ymin=lo_90, ymax=hi_90)) +
        geom_point(size=3, color="#0000CC") + geom_errorbar(width=0.25, color="#0000CC") + geom_abline(slope=0, intercept=0, lty=2) +
        geom_errorbar(data=corr_metric, aes(project, spearman_estimate, ymin=lo_95, ymax=hi_95), width=0.5, color="#0000CC") +
        ylim(c(-1,1)) + theme_bw() + labs(title=corr_metric$metric[1], x="Projet", y="Coefficient de correlation")
    )
    dev.off()
  })


  corr_count_data = merge(spearman_tests(metrics_6m, mnames, "BugFixes"), spearman_bootstrap(metrics_6m, mnames, "BugFixes"))
  by(corr_count_data, corr_count_data$metric, function(corr_metric) {
    cairo_pdf(paste0(dt_env$working_dir,"/", "corr_count_", corr_metric$metric[1],".pdf"), width = 5, height = 5)
    print(
      ggplot(corr_metric, aes(project, spearman_estimate, ymin=lo_90, ymax=hi_90)) +
        geom_point(size=3, color="#0000CC") + geom_errorbar(width=0.25, color="#0000CC") + geom_abline(slope=0, intercept=0, lty=2) +
        geom_errorbar(data=corr_metric, aes(project, spearman_estimate, ymin=lo_95, ymax=hi_95), width=0.5, color="#0000CC") +
        ylim(c(-1,1)) + theme_bw() + labs(title=corr_metric$metric[1], x="Projet", y="Coefficient de correlation")
    )
    dev.off()
  })


  library(relaimpo)
  model = BugFixes ~ LoC + Churn + NumDev + MVO + Major +  MAF
  by(metrics_6m, metrics_6m$project, function (x) {
    linmod = lm(model, data=x)
    #summary(linmod)
    relimp = calc.relimp(linmod, type ="pmvd")
    plot_pmvd(relimp, dt_env$projects_names[[x$project[1]]])
  })
}
