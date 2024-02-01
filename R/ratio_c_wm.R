
library(tidyverse)



find_c_wm_ratio <- function(x){
    df_traits <- readr::read_rds(system.file("extdata", "bray_traits_c_wm_ratio.rds", package = "traitfinder"))
  
  # return("This is it")
  ###
  # function to extract the feeding trait from the WORMS database
  # x is the species name
  # returns the mean c_wm_ratio feeding trait
  # if no match is found returns NA
  # todo: include rank in the output
  
  
      x = gsub("\\(.*\\)", "", x)
      x = stringr::str_remove_all(x, "[^[:alpha:][:space:]]")
      x = stringr::str_split(x, "\\[")[[1]][1]
      x = stringr::str_replace_all(x, "\\\\", "")
      # trim x
      x = stringr::str_trim(x)
      # remove text strings that are within brackets in x
      x = stringr::str_remove_all(x, "\\[.*\\]")
      x = stringr::str_remove_all(x, "\\(.*\\)")
      # remove numbers from x
      x = stringr::str_remove_all(x, "[0-9]")
      # remove special characters from x
      x = stringr::str_remove_all(x, "[^[:alnum:][:space:]]")
      # remove all non-alphabetic characters from x
      x = stringr::str_replace(x, "  ", " ")
      # remove instances of species labels
      x = stringr::str_remove_all(x, "(?<![[:alpha:]])sp(?![[:alpha:]])")
      
      # get rid of \ in x
      x = stringr::str_trim(x)
      
        bad = TRUE
      tryCatch({
          df_worms <- worrms::wm_records_name(name = x) 
          df_worms <- dplyr::filter(df_worms, is.na(scientificname) == FALSE)
        if (nrow(df_worms) > 0)
          bad <- FALSE
      }, error = function(e){
        bad = TRUE
      })
      if (bad)
        return (
          dplyr::tibble(c_wm_ratio = -9999,
                 n_taxon = NA, rank = NA, name_matched = NA, source = "Unable to find any matches in WORMS")
        )
      
      df_worms <- dplyr::slice(df_worms, 1)
      # check if the status was deleted
      if (df_worms$status[1] == "deleted")
          return(dplyr::tibble(c_wm_ratio = -9999,
                 n_taxon = NA, rank = NA, name_matched = NA, source = "Unable to find any matches in WORMS"))
      
      rank_name <- df_worms$rank
      
      if (rank_name == "Kingdom")
          return(dplyr::tibble(c_wm_ratio = -9999,
                 n_taxon = NA, rank = NA, name_matched = NA, source = "Unable to find any matches in WORMS"))
      
      candidates <- dplyr::select(df_worms,phylum, class, order, family, genus)
      candidates <- tidyr::gather(candidates, key, value, phylum:genus) 
      candidates <-  dplyr::slice(candidates, 5:1) 
      candidates <- tidyr::drop_na(candidates) 
      candidates <- candidates$value
      
      df_worms <- dplyr::select(df_worms, valid_name)
      
      names(df_worms) <- rank_name
      
      i <- 0
      while (TRUE){
      
      
      if (i > 0){
        x <- candidates[i]
        
      }
      i <- i + 1
      
      df_worms <- worrms::wm_records_name(name = x)
      df_worms <- dplyr::filter(df_worms, is.na(scientificname) == FALSE) 
      df_worms <- dplyr::slice(df_worms, 1)
      
      rank_name <- df_worms$rank
      
      if ((rank_name %in% c("Suborder", "Parvphylum", "Subfamily", "Subspecies", "Kingdom")) == FALSE){
      df_worms <- dplyr::select(df_worms, valid_name)
    
      valid_name <- df_worms$valid_name
      
      names(df_worms) <- rank_name
      
      if ("Subclass" %in% names(df_worms))
        df_worms <- dplyr::rename(df_worms, Order = Subclass) 
      
      # make all column names lower case
      df_worms <- dplyr::rename_all(df_worms, tolower)
      
      suppressMessages(df_beau <- df_worms %>% 
        dplyr::select_if(~ !any(is.na(.))) %>% 
        dplyr::inner_join(df_traits) )
      if (nrow(df_beau) > 0) {
        return(
          df_beau %>% 
            dplyr::filter(is.na(c_wm_ratio) == FALSE) %>%
            dplyr::summarize(c_wm_ratio = mean(c_wm_ratio, na.rm = TRUE), n_taxon = dplyr::n()) %>%
              dplyr::mutate(rank = rank_name) %>% 
              dplyr::mutate(name_matched = valid_name) %>% 
              dplyr::mutate(source = "Bray" )
        )
      # }
      
      
      }
      
      if (i > length(candidates))
          return(dplyr::tibble(c_wm_ratio = -9999,
                 n_taxon = NA, rank = NA, name_matched = NA, source = "Unable to find any matches in WORMS"))
      
      }
      }
}

# find_c_wm_ratio("Amphiura")








