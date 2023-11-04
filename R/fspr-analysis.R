#' Creates a screenshot analysis summaries for
#'
#' @param fspr_obj fspr screenshots object
#'
#' @return data.frame
#' @export
#'
#' @examples
create_screenshot_summaries <- function(fspr_obj) {
  if (!("fspr.screenshots" %in% class(fspr_obj))){
    warning("this is not a screenshot object")
    return(NULL)
  }
  df_res <- data.frame()
  i_screenshots <- unique(fspr_obj$scene_analysis$iAnalysis)
  tProgress <- txtProgressBar(max = length(i_screenshots), style = 3)
  for (i in seq_len(length(i_screenshots))){
    df_screenshot <- create_screenshot_summary(fspr_obj, i_screenshots[i])
    df_res <- rbind(df_res, df_screenshot)
    setTxtProgressBar(tProgress, i)
  }
  close(tProgress)
  return(df_res)
}

#' @export
#' @describeIn create_screenshot_summaries Creates a single summary based on
#' given screenshot index. In case there are no objects to be matched (only nothing
#' has been detected in the screenshot), returns zero line data.frame with proper names
#'
create_screenshot_summary <- function(fspr_obj, i_screenshot){
  # distance from all the objects
  df_res <- filter_screenshot(fspr_obj$scene_analysis, i_screenshot)
  if (is.null(df_res)) return(NULL)

  df_res <- df_res %>%
    left_join(select(fspr_obj$screen_position, -time),
              by = c("iAnalysis", "object")) %>%
    filter(object != "nothing")

  if (nrow(df_res) == 0) {
    dummy_df <- mutate(df_res, camera_distance =  NA_real_)
    return(dummy_df)
  }

  ## Camera position in the world at the time of the screenshot
  camera_position <- filter(fspr_obj$position, iAnalysis == i_screenshot) %>%
    select(starts_with("position")) %>%
    unlist()

  # Calculates distances of each objecttowards the camera
  df_distances <- fspr_obj$object_positions %>%
    filter(object %in% df_res$object) %>%
    rowwise() %>%
    mutate(camera_distance = euclid_distance(c(position_x, position_y),
                                             camera_position[1:2])) %>%
    select(object, camera_distance) %>%
    ungroup()

  df_res <- right_join(df_distances, df_res, by = "object")
  return(df_res)
}
