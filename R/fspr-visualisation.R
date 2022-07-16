#' plots position of objects on the screen with approximate size as per their
#'  visual size
#'
#' @param fspr_obj
#' @param i_screenshot
#'
#' @return
#' @export
#'
#' @examples
plot_screenshot_scene <- function(fspr_obj, i_screenshot, width = 0.1){
  df_screenshot <- get_screenshot_object_screen_position(fspr_obj, i_screenshot)
  w <- width
  plt <- df_screenshot %>%
  ggplot(aes(x = screen_x, y = screen_y, fill = object)) +
    geom_point() +
    geom_rect(aes(xmin = screen_x - (ratio), xmax = screen_x + (ratio),
                  ymin = screen_y - (ratio), ymax = screen_y + (ratio))) +
    ggrepel::geom_text_repel(aes(label=object)) +
    lims(x = c(-0.5, 1.5), y = c(-0.5, 1.5))
  return(plt)
}

plot_screenshot_analysis <- function(fspr_obj, i_screenshot){

}

#' Plots position and object data
#'
#' @param fspr_obj
#'
#' @return
#' @export
#'
#' @examples
plot_positions_and_path <- function(fspr_obj, i_screenshot = NULL){
  plt <- ggplot(fspr_obj$object_positions, aes(position_x, position_y)) +
    geom_path(data = fspr_obj$position) +
    geom_point() +
    ggrepel::geom_text_repel(aes(label=object)) +
    geom_point(aes(x = fspr_obj$position$position_x[1],
                   y = fspr_obj$position$position_y[1]),
               shape=18, size = 10)
  if(!is.null(i_screenshot) && is.numeric(i_screenshot)){
    plt <- plt +
      geom_point(aes(x = fspr_obj$position$position_x[i_screenshot],
                     y = fspr_obj$position$position_y[i_screenshot]),
                 shape=20, size = 10, color = "pink")
  }
  return(plt)
}
