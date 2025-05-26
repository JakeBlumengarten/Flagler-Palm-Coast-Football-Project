cfb <- data.frame()
seasons <- 2014:cfbfastR:::most_recent_cfb_season()
progressr::with_progress({
  
  cfb <- cfbfastR::load_cfb_pbp(seasons)
})
tictoc::toc()