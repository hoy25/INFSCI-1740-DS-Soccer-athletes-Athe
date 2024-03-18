### the GLOBAL script for the JSON upload app

# imports

library(shiny)

library(tidyverse)

library(jsonlite)


### modify the max allowed upload size -- 30 MB
options(shiny.maxRequestSize = 30*1024^2)

### global functions

# function to extract a single game from the data
get_game <- function(selected_index, games_data) {
  return(games_data %>% pluck(selected_index))
}


# generate a data dictionary for the given game
get_game_dict <- function(selected_index, games_data) {
  tibble::tibble(
    flattened_variable_name = get_game(selected_index, games_data) %>% names(),
    data_type = map_chr( get_game(selected_index, games_data), class )
  ) %>% 
    mutate(count_num_periods = stringr::str_count(flattened_variable_name, '\\.')) %>% 
    tidyr::separate(flattened_variable_name,
                    c("variable_group", "variable_name"),
                    sep = '\\.',
                    remove = FALSE,
                    extra = 'merge',
                    fill = 'right') %>% # suppresses warning 
    mutate(variable_name = ifelse( count_num_periods == 0, 
                                   flattened_variable_name,
                                   variable_name) ) %>% 
    mutate(variable_group = ifelse( count_num_periods == 0,
                                    'event_information',
                                    variable_group ) ) %>% 
    select(-count_num_periods)
}

# function to extract the list values of one row and column for a single game
# (intended to be used for mapping across an entire game)

extract_list_values_per_row <- function(rowid, list_column_name, single_game)
{
  one_row <- single_game %>% 
    select(all_of(list_column_name)) %>% 
    slice(rowid)
  
  one_vector <- one_row %>% pull() %>% unlist()
  
  res <- list( x = one_vector, 
               rowid = rep( rowid, length(one_vector) ) )
  
  names(res) <- c(list_column_name, 'rowid')
  
  res
}