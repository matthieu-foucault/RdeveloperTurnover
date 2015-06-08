
dt_env = new.env()
developer_turnover = function(database_host, working_dir, web_working_dir = working_dir) {
  library(reshape2)
  library(ggplot2)
  library(gplots)
  library(RColorBrewer)
  library(gdata)
  library(grid)
  library(gridExtra)
  library(htmlTable)

  dt_env$database_host = database_host
  dt_env$working_dir = working_dir
  dt_env$web_working_dir = web_working_dir

  projects_names = new.env(hash=T, parent=emptyenv())
  projects_names[["https://github.com/matthieu-foucault/jquery.git"]] = "JQuery"
  projects_names[["https://github.com/rails/rails.git"]] = "Rails"
  projects_names[["https://github.com/jenkinsci/jenkins.git"]] = "Jenkins"
  projects_names[["https://github.com/ansible/ansible.git"]] = "Ansible"
  projects_names[["https://github.com/angular/angular.js.git"]] = "Angular.JS"
  #projects_names[["https://github.com/mono/mono.git"]] = "Mono"
  dt_env$projects_names = projects_names

  S0 = new.env(hash=T, parent=emptyenv())
  S0[["https://github.com/jenkinsci/jenkins.git"]] = "3991cd04fd13aa086c25820bdfaa9460f0810284"
  S0[["https://github.com/rails/rails.git"]] = "73fc42cc0b5e94541480032c2941a50edd4080c2"
  S0[["https://github.com/matthieu-foucault/jquery.git"]] = "95559f5117c8a21c1b8cc99f4badc320fd3dcbda"
  S0[["https://github.com/ansible/ansible.git"]] = "6221a2740f5c3023c817d13e4a564f301ed3bc73"
  S0[["https://github.com/angular/angular.js.git"]] = "519bef4f3d1cdac497c782f77457fd2f67184601"

  dir.create(working_dir, showWarnings =F)
  dir.create(web_working_dir, showWarnings =F)


  cat("loading from database... "); flush.console()
  load_developers_activity()
  cat(" DONE\n"); flush.console()
  cat("plotting turnover evolution... "); flush.console()
  plot_turnover_evolution()
  cat("DONE\n"); flush.console()
  cat("plotting period selection results... "); flush.console()
  plot_dev_sets_comparison()
  cat("DONE\n"); flush.console()
  cat("computing metrics and correlation with quality... "); flush.console()
  compute_correlations()
  cat("DONE\n"); flush.console()
}
