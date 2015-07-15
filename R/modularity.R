
get_modules = function(db_url, file, community_function=fastgreedy.community) {
  library(mongolite)

  commits_files = mongo("commits-files", "developers_activity",paste0("mongodb://", db_url))$find()

  library(reshape2)
  library(igraph)

  proj_modules_regexes = by(commits_files, commits_files$project, function(proj_commits_files) {
    w = dcast(proj_commits_files, file~commit, length)
    x <- as.matrix(w[,-1])
    x[is.na(x)] <- 0
    x <- apply(x, 2,  function(x) as.numeric(x > 0))  #recode as 0/1
    v <- x %*% t(x)                                   #the magic matrix
    diag(v) <- 0                                      #replace diagonal
    dimnames(v) <- list(w[, 1], w[,1])

    g = graph.adjacency(v,mode="undirected", weighted=TRUE)
    modules = membership(community_function(g))
    modules_regexes = lapply(c(1:max(modules)), function(mod_id) {
      paste0('"', gsub('\\.', '\\\\\\\\.', paste0("^(",paste(names(modules[modules == mod_id]), collapse="|"), ")")), '"')
    })
    paste0('"',proj_commits_files$project[1],'":{ "modules":[', paste(modules_regexes, collapse=",\n"), ']}' )
  })


  cat(paste('{', paste(proj_modules_regexes, collapse=","), '}'), file=file)
}
