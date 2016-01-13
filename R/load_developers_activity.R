
parseMongoDate = function(x) {
  as.Date(
    gsub("[^\\s]* ([^\\s]* [^\\s]* [^\\s]*) [^\\s]* ([^\\s]*)",
         "\\1 \\2",x)
    , format = "%b %d %H:%M:%S %Y")
}



load_developers_activity = function() {
  with(dt_env, {
    library(mongolite)

    Sys.setlocale("LC_TIME", "English")

    url = paste0("mongodb://", database_host)

    #act_months_after = dbGetQuery(mongo, "developer_activity_months_after", "", skip=0, limit=0)
    act_months_after = mongo("developer_activity_months_after", database_name, url, F)$find()
    act_months_after$releaseDate = as.Date(act_months_after$releaseDate)
    act_months_after$X_id = NULL
    dt_env$act_months_after = act_months_after

    act_months_before = mongo("developer_activity_months_before", database_name, url, F)$find()
    act_months_before$releaseDate = as.Date(act_months_before$releaseDate)
    act_months_before$X_id = NULL
    act_months_before = act_months_before

#     act_releases = mongo("developer_activity_release", database_name, url, F)$find()
#     act_releases$releaseDate = as.Date(act_releases$releaseDate)
#     act_releases$X_id = NULL
#     dt_env$act_releases = act_releases

    mod_m = mongo("modules_metrics", database_name, url, F)$find()
    #remove modules with 0 LoC
    mod_m = subset(mod_m, LoC > 0)
    mod_m[is.na(mod_m)] = 0
    mod_m$BugDensity = mod_m$BugFixes / mod_m$LoC
    mod_m$X_id = NULL
    dt_env$mod_m = mod_m

    commit_dates = mongo("devs_commit_dates", database_name, url, F)$find()
    commit_dates$time = as.Date(commit_dates$time)
    commit_dates$X_id = NULL
    commit_dates = subset(commit_dates, time > as.Date("2005-01-01"))
    dt_env$commit_dates = commit_dates

  })
}
