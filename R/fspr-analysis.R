#' Creates a screenshot analysis summaries for
#'
#' @param fspr_obj fspr screenshots object
#'
#' @return data.frame
#' @export
#'
#' @examples
create_screenshot_summaries <- function(fspr_obj){
  if (!("fspr.screenshots" %in% class(fspr_obj))){
    warning("this is not a screenshot object")
    return(NULL)
  }
  df_res <- data.frame()
  for (i_screenshot in unique(fspr_obj$scene_analysis$iAnalysis)){
    df_screenshot <- create_screenshot_summary(fspr_obj, i_screenshot)
    df_res <- rbind(df_res, df_screenshot)
  }
  return(df_res)
}

#' @export
#' @describeIn create_screenshot_summaries Creates a single summary based on
#' given screenshot index
create_screenshot_summary <- function(fspr_obj, i_screenshot){
  # distance from all the objects
  df_res <- filter_screenshot(fspr_obj$scene_analysis, i_screenshot) %>%
    left_join(select(fspr_obj$screen_position, -time),
               by = c("iAnalysis", "object")) %>%
    filter(object != "nothing")

  camera_position <- filter(fspr_obj$position,
         iAnalysis == i_screenshot) %>%
    select(starts_with("position")) %>%
    as.vector()

  df_res <- fspr_obj$object_positions %>%
    filter(object %in% df_res$object) %>%
    rowwise() %>%
    mutate(camera_distance = euclid_distance(c(position_x, position_y),
      camera_position[1:2])) %>%
    select(object, camera_distance) %>%
    right_join(df_res, by="object") %>%
    ungroup()
  return(df_res)
}
