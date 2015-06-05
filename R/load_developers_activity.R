
parseMongoDate = function(x) {
  as.Date(
    gsub("[^\\s]* ([^\\s]* [^\\s]* [^\\s]*) [^\\s]* ([^\\s]*)",
         "\\1 \\2",x)
    , format = "%b %d %H:%M:%S %Y")
}



load_developers_activity = function() {
  with(dt_env, {
    library(RMongo)

    Sys.setlocale("LC_TIME", "English")

    mongo = mongoDbConnect("developers_activity", host=database_host)

    act_months_after = dbGetQuery(mongo, "developer_activity_months_after", "", skip=0, limit=0)
    act_months_after$releaseDate = parseMongoDate(act_months_after$releaseDate)
    act_months_after$X_id = NULL
    dt_env$act_months_after = act_months_after

    act_months_before = dbGetQuery(mongo, "developer_activity_months_before", "", skip=0, limit=0)
    act_months_before$releaseDate = parseMongoDate(act_months_before$releaseDate)
    act_months_before$X_id = NULL
    act_months_before = act_months_before

    act_releases = dbGetQuery(mongo, "developer_activity_release", "", skip=0, limit=0)
    act_releases$releaseDate = parseMongoDate(act_releases$releaseDate)
    act_releases$X_id = NULL
    dt_env$act_releases = act_releases

    mod_m = dbGetQuery(mongo, "modules_metrics", "", skip=0, limit=0)
    #remove modules with 0 LoC
    mod_m = subset(mod_m, LoC > 0)
    mod_m[is.na(mod_m)] = 0
    mod_m$BugDensity = mod_m$BugFixes / mod_m$LoC
    mod_m$X_id = NULL
    dt_env$mod_m = mod_m

    commit_dates = dbGetQuery(mongo, "devs_commit_dates", "", skip=0, limit=0)
    commit_dates$time = parseMongoDate(commit_dates$time)
    commit_dates$X_id = NULL
    commit_dates = subset(commit_dates, time > as.Date("2005-01-01"))
    dt_env$commit_dates = commit_dates

    dbDisconnect(mongo)
  })
}
