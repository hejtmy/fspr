# Helpers -----------
#' turns vector columns in string "(x, y, z)" into three
#' columns(position_x, position_y, position_z) and returns the table
vector3_to_columns <- function(df_position, column, flip_yz = TRUE, remove=TRUE){
  if(!requireNamespace("stringr", quietly = TRUE)){
    print("Cannot continue withouth stringr package. Please install it")
    return(FALSE)
  }
  ifelse(flip_yz, xyz <- c("x", "z", "y"), xyz <- c("x", "y", "z"))
  #TODO - remove the data.table
  positions <- df_position[[column]]
  pos <- stringr::str_match(positions, "\\((.*),(.*),(.*)\\)")[, 2:4]
  pos <- as.data.frame(pos)
  new_names <- stringr::str_c(column, sep="_", xyz)
  colnames(pos) <- new_names
  pos <- mutate(pos, across(everything(), as.numeric))
  df_position <- tibble::add_column(df_position, pos, .after = column)
  if(remove){
    df_position <- select(df_position, -column)
  }
  return(df_position)
}
