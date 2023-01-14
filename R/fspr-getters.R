get_screenshot_data <- function(fspr_obj, i_screenshot){
  UseMethod("get_screenshot_data")
}

get_screenshot_data.fspr.screenshots <- function(fspr_obj, i_screenshot) {
  new_obj <- fspr_obj
  fields <- c("screen_position", "scene_analysis", "position")
  for (field in fields) {
    new_obj[[field]] <- filter_screenshot(fspr_obj[[field]], i_screenshot)
  }
  return(new_obj)
}

filter_screenshot <- function(obj, i_screenshot){
  if (!("iAnalysis" %in% names(obj))) return(NULL)
  return(obj[obj$iAnalysis == i_screenshot, ])
}


#' @description This function takes as input an fspr object and filters
#' it to include onlythe objects that are visible in the scene.
#' It does this by taking the objects that are visible in the scene
#'
#' @param fspr_obj fspr object
#' @return filtered fspr object
#' @export 

filter_non_visible <- function(fspr_obj) {
  objects <- fspr_obj$scene_analysis$object
  fspr_obj$screen_position <- filter(fspr_obj$screen_position, object %in% objects)
  fspr_obj$object_positions <- filter(fspr_obj$object_position, object %in% objects)
  return(fspr_obj)
}