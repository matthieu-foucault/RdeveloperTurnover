get_conversion_rate = function() {
  with(dt_env, {

    s = by(commit_dates, commit_dates$project, function(ddp) {
      first = min(ddp$time) + 182
      last = max(ddp$time) - 182
      all_stayers = NULL
      stayers_leavers = NULL
      evol = do.call(rbind, lapply(seq(first, last, 182), function(t) {
        devsbefore = unique(subset(ddp, time < t & time > (t - 182))$author)
        devsafter = unique(subset(ddp, time > t & time < (t + 182))$author)

        stayers = intersect(devsbefore,devsafter)
        all_stayers <<- c(all_stayers, stayers)
        stayers_leavers <<- c(stayers_leavers, setdiff(all_stayers, stayers))
        all = unique(union(devsbefore,devsafter))
        newcomers = setdiff(devsafter, devsbefore)
        leavers = setdiff(devsbefore, devsafter)
        data.frame(time = t + 182, tot = length(all), s = length(stayers), n = length(newcomers), l = length(leavers))
      }))

      name_p = projects_names[[ddp$project[1]]]
      #
      #   d = melt(evol[,-2],id.vars="time")
      #
      #   cairo_pdf(paste0("evol-",name_p,"_area.pdf"),width=2.75, height=2.75)
      #   print(ggplot(d, aes(time,value)) + geom_area(aes(colour=variable, fill=variable),position="stack"))
      #   dev.off()

      s_1 = releases_dates[[name_p]] - 182

      devsbefore = unique(subset(ddp, time < s_1 & time > (s_1 - 182))$author)
      devsafter = unique(subset(ddp, time > s_1 & time < (s_1 + 182))$author)

      newcomers = setdiff(devsafter, devsbefore)

      real_stayers = subset(table(all_stayers), table(all_stayers) >= 1)

      s0_stayers = intersect(devsbefore, names(real_stayers))

      ndevs = length(unique(ddp$author))
      list(all_stayers=all_stayers, stayers_leavers=stayers_leavers,
           overall_conversion_rate=length(real_stayers)/ndevs, s0_conversion_rate=length(s0_stayers)/length(devsbefore),
           nnew=length(newcomers), nstay=length(s0_stayers), devsbefore=devsbefore, devsafter=devsafter, newcomers=newcomers )
    })
  })
}
