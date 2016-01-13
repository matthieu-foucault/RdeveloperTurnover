plot_pmvd = function(relimp, project_name) {
  ylab = "% of response variance"
  cairo_pdf(filename = paste0(dt_env$working_dir,"/pmvd_", project_name, ".pdf"), width = 5, height = 4)
  par(mar = c(2, 4.1, 1.5, 0), oma = c(1.5, 0, 0, 0))
  xpos = barplot(100 * relimp@pmvd, main=project_name, ylab=ylab, names.arg="", ylim = c(0,100))
  text(xpos + 0.25, par("usr")[3], srt=45, adj=1,
       labels=paste(gsub(".*Dev.*","#Devs",names(relimp@pmvd))," "),
       xpd=T)
  reltext="%"
  mtext(bquote(R^2==.(100*round(relimp@R2,4)) * .(eval(reltext))), side = 1, line=0.5, outer=T, cex=1, adj=0.5)
  dev.off()
}

relative_importance = function() {
  library(relaimpo)

  model = BugFixes ~ LoC + A + NumDevsMean + ENA + INA + StA
  by(metrics_6m, metrics_6m$project, function (x) {
    linmod = lm(model, data=x)
    #summary(linmod)
    relimp = calc.relimp(linmod, type ="pmvd")
    plot_pmvd(relimp, dt_env$projects_names[[x$project[1]]])
  })

}
