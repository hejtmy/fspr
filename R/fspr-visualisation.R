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
    lims(x=c(-0.5,1.5),y=c(-0.5,1.5))
  return(plt)
}

plot_screenshot_analysis <- function(fspr_obj, i_screenshot){

}