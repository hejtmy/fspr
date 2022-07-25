#' plots position of objects on the screen with approximate size as per their
#'  visual size
#'
#' @param fspr_obj object to plot
#' @param i_screenshot which screenshot to plot
#' @param include_screenshot should the oriignal screenshot be included
#' @param screenshots_path if screenshot should be included, what folder are the
#' screenshots placed in? It will be searched for automatically based on i_screenshot
#'
#' @return
#' @export
#'
#' @examples
plot_screenshot_scene <- function(fspr_obj, i_screenshot, include_screenshot = FALSE,
                                  screenshots_path = NULL){
  df_screenshot <- get_screenshot_object_screen_position(fspr_obj, i_screenshot)
  plt <- ggplot(df_screenshot, aes(x = screen_x, y = screen_y, fill = object))
  if(include_screenshot){
    g <- get_screenshot_raster(screenshots_path, i_screenshot)
    plt <- plt +
      annotation_custom(g, xmin=0, xmax=1, ymin=0, ymax=1)
  }
  plt <- plt +
    geom_point() +
    geom_rect(aes(xmin = screen_x - (ratio), xmax = screen_x + (ratio),
                  ymin = screen_y - (ratio), ymax = screen_y + (ratio))) +
    ggrepel::geom_text_repel(aes(label=object)) +
    expand_limits(x = c(0,1), y=c(0,1))
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

get_screenshot_raster <- function(base_path, index){
  ptr <- sprintf("HighresScreenshot%05d", index)
  filepath <- list.files(base_path,pattern = ptr, full.names = TRUE)
  if(length(filepath) == 0){
    warning("There is no file ", filepath, " at ", base_path)
    return(NULL)
  }
  png <- png::readPNG(filepath)
  g <- grid::rasterGrob(png, interpolate=TRUE, width = unit(1, "npc"), height = unit(1, "npc"))
  return(g)
}


