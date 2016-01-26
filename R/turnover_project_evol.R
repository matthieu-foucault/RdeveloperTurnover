
project_turnover = function(commit_dates, t_i, period_in_days = 182) {
	devsbefore = unique(subset(commit_dates, time < (t_i - period_in_days) & time > (t_i - 2*period_in_days))$author)
	devsafter = unique(subset(commit_dates, time > (t_i - period_in_days) & time < t_i)$author)

	stayers = intersect(devsbefore,devsafter)
	if (exists("all_stayers"))	all_stayers <<- c(all_stayers, stayers)
	if (exists("stayers_leavers")) stayers_leavers <<- c(stayers_leavers, setdiff(all_stayers, stayers))
	all = unique(union(devsbefore,devsafter))
	newcomers = setdiff(devsafter, devsbefore)
	leavers = setdiff(devsbefore, devsafter)
	data.frame(time = t_i, tot = length(all), s = length(stayers), n = length(newcomers), l = length(leavers))
}


plot_turnover_evolution = function() {
  with(dt_env, {
    releases_dates = new.env(hash=T, parent=emptyenv())
    releases_dates[["JQuery"]] = as.Date("2012-08-09")
    releases_dates[["Rails"]] = as.Date("2009-03-15")
    releases_dates[["Jenkins"]] = as.Date("2013-04-01")
    releases_dates[["Ansible"]] = as.Date("2014-02-28")
    releases_dates[["Angular.JS"]] = as.Date("2012-06-14")
    releases_dates[["Mono"]] = as.Date("2011-02-14")
    releases_dates[["PHPUnit"]] = as.Date("2011-10-31")

    start_dates = new.env(hash=T, parent=emptyenv())
    start_dates[["JQuery"]] = as.Date("2011-11-15")
    start_dates[["Rails"]] = as.Date("2009-03-15")
    start_dates[["Jenkins"]] = as.Date("2011-11-27")
    start_dates[["Ansible"]] = as.Date("2013-02-23")
    start_dates[["Angular.JS"]] = as.Date("2011-01-06")
    start_dates[["Mono"]] = as.Date("2011-02-14")
    start_dates[["PHPUnit"]] = as.Date("2011-01-01")



    s = by(commit_dates, commit_dates$project, function(commit_dates_proj) {
      first = min(commit_dates_proj$time) + 2*182
      last = max(commit_dates_proj$time)
      all_stayers = NULL
      stayers_leavers = NULL
      evol = do.call(rbind, lapply(seq(first, last, 15), function(t) {
      	project_turnover(commit_dates_proj, t)
      }))

      name_p = projects_names[[commit_dates_proj$project[1]]]
      dir.create(paste0(dt_env$web_working_dir,"/results"), showWarnings =F)

      cairo_pdf(paste0(working_dir, "/evol-",name_p,".pdf"),width=5, height=5)
      par(mar=c(2,2,2,1), cex =1.5)
      lw = 5
      plot(evol$time, evol$tot,
           col="#377eb8", type="l", lwd=lw, main=name_p, ylab="#developers",
           xlab="Date", ylim= c(0, max(evol$tot)), xlim=c(start_dates[[name_p]], max(evol$time)))
      lines(evol$time, evol$l, col="#e41a1c", lty=2, lwd=lw)
      lines(evol$time, evol$n, col="#4daf4a", lty=15, lwd=lw)
      lines(evol$time, evol$s, col="#984ea3", lty=1, lwd=lw)
      abline(v=releases_dates[[name_p]], lty=2)
      dev.off()

      svg(paste0(web_working_dir, "/results/evol-",name_p,".svg"),width=5, height=5)
      par(mar=c(2,2,2,1), cex =1.5)
      lw = 5
      plot(evol$time, evol$tot,
           col="#377eb8", type="l", lwd=lw, main=name_p, ylab="#developers",
           xlab="Date", ylim= c(0, max(evol$tot)), xlim=c(start_dates[[name_p]], max(evol$time)))
      lines(evol$time, evol$l, col="#e41a1c", lty=2, lwd=lw)
      lines(evol$time, evol$n, col="#4daf4a", lty=15, lwd=lw)
      lines(evol$time, evol$s, col="#984ea3", lty=1, lwd=lw)
      abline(v=releases_dates[[name_p]], lty=2)
      dev.off()

      cairo_pdf(paste0(working_dir, "/evol-",name_p,"-ratio.pdf"),width=5, height=5)
      par(mar=c(2,2,2,1), cex =1.5)
      lw = 5
      plot(evol$time, evol$l/evol$tot, col="#e41a1c", type="l", lwd=lw, main=name_p, ylab="Developers Ratio",
           xlab="Date", ylim= c(0, 1), xlim=c(start_dates[[name_p]], max(evol$time)))
      lines(evol$time, evol$n/evol$tot, col="#4daf4a", lty=15, lwd=lw)
      lines(evol$time, evol$s/evol$tot, col="#984ea3", lty=1, lwd=lw)
      abline(v=releases_dates[[name_p]], lty=2)

      dev.off()

      svg(paste0(web_working_dir, "/results/evol-",name_p,"-ratio.svg",sep=""),width=5, height=5)
      par(mar=c(2,2,2,1), cex =1.5)
      lw = 5
      plot(evol$time, evol$l/evol$tot, col="#e41a1c", type="l", lwd=lw, main=name_p, ylab="Developers Ratio",
           xlab="Date", ylim= c(0, 1), xlim=c(start_dates[[name_p]], max(evol$time)))
      lines(evol$time, evol$n/evol$tot, col="#4daf4a", lty=15, lwd=lw)
      lines(evol$time, evol$s/evol$tot, col="#984ea3", lty=1, lwd=lw)
      abline(v=releases_dates[[name_p]], lty=2)
      dev.off()
     })
  })
}
