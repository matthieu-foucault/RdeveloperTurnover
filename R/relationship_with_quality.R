
# Writes a correlation result in the LateX format
stringify.cor = function(cor.result, html = F) {
  if (is.na(cor.result$estimate))
    return ("0.00")
  s = as.numeric(round(cor.result$estimate, 2))
  if (cor.result$p.value < 0.001 ) {
    s = paste(s, "***")
  } else if (cor.result$p.value < 0.01 ) {
    s = paste(s, "**")
  } else if (cor.result$p.value < 0.05 ) {
    s = paste(s, "*")
  } else if (cor.result$p.value < 0.1 ) {
    s = paste(s, ".")
  }

  if (cor.result$p.value < 0.1) {
    if (html)
      s = paste0("<b>", s, "</b>")
    else
      s = paste0("\\textbf{", s, "}")
  }
  return(s)
}

cor.string = function(x, y, html=F) {
  stringify.cor(cor.test(x,y, method="spearman") , html)
}

compute_correlations = function() {
  compute_metrics = function(period_month_size) {
    do.call(rbind, lapply(ls(dt_env$projects_names), function(project_url) {
      contribs = get_contributions(project_url, period_month_size)
      turnover_metrics_project(contribs)
    }))
  }

  metrics_rel = merge(dt_env$mod_m, compute_metrics(0), by = c("module","project"), all=F)
  metrics_1m = merge(dt_env$mod_m, compute_metrics(1), by = c("module","project"), all=F)
  metrics_3m = merge(dt_env$mod_m, compute_metrics(3), by = c("module","project"), all=F)
  metrics_6m = merge(dt_env$mod_m, compute_metrics(6), by = c("module","project"), all=F)

  mnames = c("INA","ILA","ENA","ELA","SA", "A")
  compute_cor = function(metr, html=F) {
    do.call(rbind, by(metr, metr$project, function(metrics_p) {
      c(dt_env$projects_names[[metrics_p$project[1]]],lapply(metrics_p[,mnames], function(x){cor.string(metrics_p$BugDensity, x, html)}))
    }))
  }


  corr_rel = compute_cor(metrics_rel)
  corr_1m = compute_cor(metrics_1m)
  corr_3m = compute_cor(metrics_3m)
  corr_6m = compute_cor(metrics_6m)

  write.table(corr_6m, file=paste0(dt_env$working_dir,"/bugscor.tex"), sep=" & ", quote=F, row.names=F, col.names=F, eol= " \\\\\n")

  dir.create(paste0(dt_env$web_working_dir,"/results"), showWarnings =F)

  write(htmlTable(compute_cor(metrics_rel, T), rnames = F,css.class="table"), paste0(dt_env$web_working_dir,"/results/corr_rel.html"))
  write(htmlTable(compute_cor(metrics_1m, T), rnames = F,css.class="table"), paste0(dt_env$web_working_dir,"/results/corr_1m.html"))
  write(htmlTable(compute_cor(metrics_3m, T), rnames = F,css.class="table"), paste0(dt_env$web_working_dir,"/results/corr_3m.html"))
  write(htmlTable(compute_cor(metrics_6m, T), rnames = F,css.class="table"), paste0(dt_env$web_working_dir,"/results/corr_6m.html"))

  dir.create(paste0(dt_env$working_dir,"/patterns"), showWarnings =F)
  by(metrics_6m, metrics_6m$project, function(metrics_p) {
    name_p = dt_env$projects_names[[metrics_p$project[1]]]
    a = metrics_p[,c("A", "INA", "ENA", "ILA", "ELA", "SA", "BugFixes")]
    rownames(a) = metrics_p$module
    a = a[order(a$BugFixes),]
    colnames(a)[1] = "Total A"

    cairo_pdf(paste0(dt_env$working_dir,"/patterns/", name_p,"_fixes_metrics.pdf"), width = 8, height = 8)
    heatmap.2(log(as.matrix(a) + 0.01), Rowv=NA, Colv=NA , col = brewer.pal(9, "Blues"),
              margins = c(6,1), cexCol = 2, rowsep=1:nrow(a),
              sepcolor='#DEEBF7', sepwidth=c(0,0.0001),
              scale="none", main = name_p, density.info='none',
              trace='none',  key=FALSE, keysize=1.0, symkey=FALSE,
              lmat=rbind( c(3,2), c(1,4), c(0,0)), lhei=c(0.17,1.425, 0.1), lwid=(c(4,3)))
    dev.off()

    cairo_pdf(paste0(dt_env$working_dir,"/patterns/", name_p,"_fixes_metrics_nomodule", ".pdf"), width = 3, height = 8)
    heatmap.2(log(as.matrix(a) + 0.01), Rowv=NA, Colv=NA , col = brewer.pal(9, "Blues"),
            labRow = "", margins = c(6,0.3), cexCol = 2, rowsep=1:nrow(a),
            sepcolor='#DEEBF7', sepwidth=c(0,0.0001),
            scale="none", main = name_p, density.info='none',
            trace='none',  key=FALSE, keysize=1.0, symkey=FALSE,
            lmat=rbind( c(3,2), c(1,4), c(0,0)), lhei=c(0.17,1.425, 0.1), lwid=(c(4,0.1)))
    dev.off()

    svg(paste0(dt_env$web_working_dir,"/results/", name_p,"_fixes_metrics.svg"), width = 8, height = 8)
    heatmap.2(log(as.matrix(a) + 0.01), Rowv=NA, Colv=NA , col = brewer.pal(9, "Blues"),
              margins = c(6,0.3), cexCol = 2, rowsep=1:nrow(a),
              sepcolor='#DEEBF7', sepwidth=c(0,0.0001),
              scale="none", main = name_p, density.info='none',
              trace='none',  key=FALSE, keysize=1.0, symkey=FALSE,
              lmat=rbind( c(3,2), c(1,4), c(0,0)), lhei=c(0.17,1.425, 0.1), lwid=(c(4,3)))
    dev.off()
  })
}
