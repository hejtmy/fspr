#' Loads screenshot data from a folder
#'
#' @description the function expects a single recording in the folder
#' @param folder
#'
#' @return
#' @export
#'
#' @examples
load_screenshot_data <- function(folder){
  res <- list()
  timestamps <- get_folder_timestamps(folder)
  if(length(timestamps) != 1){
    warning("there are multiple logs from different recordings in the folder,
            select a timestamp")
    return(NULL)
  }
  res$position <- load_position_log(folder, timestamps[1])
  res$scene_analysis <- load_scene_analysis_log(folder, timestamps[1])
  res$screen_position <- load_screen_position_log(folder, timestamps[1])
  res$object_positions <- load_object_positions_log(folder, timestamps[1])
  return(res)
}

get_folder_timestamps <- function(folder){
  files <- list.files(folder, full.names = FALSE)
  timestamps <- stringr::str_match(files, ".*_(.*).txt")[,2]
  return(unique(timestamps))
}

load_position_log <- function(folder, timestamp){
  process_position <- function(df_log){
    df_log <- vector3_to_columns(df_log, "position", FALSE, TRUE)
    return(df_log)
  }
  return(find_and_load_log(folder, "position", timestamp, process_position))
}

load_scene_analysis_log <- function(folder, timestamp){
  df_scene <- find_and_load_log(folder, "scene-analysis", timestamp)
  df_scene <- df_scene %>%
    group_by(iAnalysis) %>%
    mutate(ratio = nHits/sum(nHits)) %>%
    ungroup()
  return(df_scene)
}

load_screen_position_log <- function(folder, timestamp){
  return(find_and_load_log(folder, "screen-position", timestamp))
}

load_object_positions_log <- function(folder, timestamp){
  return(find_and_load_log(folder, "object-positions", timestamp))

}

find_and_load_log <- function(folder, name, timestamp, process_function = NULL){
  pth <- find_log(folder, name, timestamp)
  if(is.null(pth)) return(NULL)
  df_log <- read.table(pth, sep=";", header=TRUE)
  if(is.null(process_function)) return(df_log)
  return(process_function(df_log))
}

find_log <- function(folder, name, timestamp){
  ptr <- stringr::str_glue("{name}_{timestamp}")
  files <- list.files(folder, pattern = ptr, full.names = TRUE)
  if(length(files) == 0){
    warning("there is no file fo name ", ptr)
    return(NULL)
  }
  return(files[1])
}
