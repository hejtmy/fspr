get_screenshot_data <- function(fspr_obj, i_screenshot){
  res <- list()

  res <- sapply(names(fspr_obj))
  res$postition
}

get_screenshot_object_screen_position <- function(fspr_obj, i_screenshot){
  df_position <- filter_screenshot(fspr_obj$screen_position, i_screenshot)
  df_scene <- filter_screenshot(fspr_obj$scene_analysis, i_screenshot)
  df_position <- df_position[df_position$object %in% df_scene$object,]
  df_position <- merge(df_position, select(df_scene, object, nHits, ratio))
  return(df_position)
}


filter_screenshot <- function(obj, i_screenshot){
  if(!("iAnalysis" %in% names(obj))) return(NULL)
  return(obj[obj$iAnalysis == i_screenshot,])
}
