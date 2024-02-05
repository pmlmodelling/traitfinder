
# read from package data
# read from package data
# use package data instead



find_trait_explorer <- function(x, trait = ""){
  # some error handling
  if (trait == "")
    return("Please specify a trait")
  
  # check trait is valid
  if (trait %in% c("dm_wm_ratio", "c_wm_ratio") == FALSE)
    return("Please specify a valid trait")
  
  if (trait == "dm_wm_ratio")
    df_traits <- readr::read_rds(system.file("extdata", "bray_traits_dm_wm_ratio.rds", package = "traitfinder"))
  if (trait == "c_wm_ratio")
    df_traits <- readr::read_rds(system.file("extdata", "bray_traits_c_wm_ratio.rds", package = "traitfinder"))
  
  names(df_traits)[1] <- "trait" 


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
          dplyr::tibble(trait = -9999,
                 n_taxon = NA, rank = NA, name_matched = NA, source = "Unable to find any matches in WORMS")
        )
      
      df_worms <- dplyr::slice(df_worms, 1)
      # check if the status was deleted
      if (df_worms$status[1] == "deleted")
          return(dplyr::tibble(trait = -9999,
                 n_taxon = NA, rank = NA, name_matched = NA, source = "Unable to find any matches in WORMS"))
      
      rank_name <- df_worms$rank
      
      if (rank_name == "Kingdom")
          return(dplyr::tibble(trait = -9999,
                 n_taxon = NA, rank = NA, name_matched = NA, source = "Unable to find any matches in WORMS"))
      
      # candidates <- dplyr::select(df_worms,phylum, class, order, family, genus)
      candidates <- dplyr::select(df_worms, genus, family, order, class, phylum)
      candidates <- tidyr::gather(candidates, key, value, phylum:genus) 
      candidates <- dplyr::filter(candidates, !str_detect(value, "unassigned"))
      candidates <- candidates %>% 
        spread(key, value)
      candidates <- dplyr::select(candidates, contains("genus"), contains("family"), contains("order"), contains("class"), contains("phylum"))
      
      df_worms <- dplyr::select(df_worms, valid_name)
      
      names(df_worms) <- rank_name
      
      i <- 1
      while (TRUE){
      
      bad <- FALSE
        if (i == 1){
      df_worms <- dplyr::slice(df_worms, 1)
      
       if ((rank_name %in% c("Suborder", "Parvphylum", "Subfamily", "Subspecies", "Kingdom", "Superfamily")) == TRUE)
         bad <- TRUE
       if ((rank_name %in% c("Suborder", "Parvphylum", "Subfamily", "Subspecies", "Kingdom", "Superfamily")) == FALSE){
      
      if ("Subclass" %in% names(df_worms))
        df_worms <- dplyr::rename(df_worms, Order = Subclass) 
      
      # make all column names lower case
      df_worms <- dplyr::rename_all(df_worms, tolower)
       }
        }
      if (i > 1){
        df_worms <- candidates %>% 
        select(i)
        rank_name <- names(df_worms)[1]
      }
      
      if (bad == FALSE){
      suppressMessages(df_beau <- df_worms %>% 
        dplyr::select_if(~ !any(is.na(.))) %>% 
        dplyr::inner_join(df_traits) )
      if (nrow(df_beau) > 0) {
        # pull the first column
        valid_name <- df_beau %>% 
          dplyr::select(1) %>% 
          slice(1) %>% 
          pull(.)
        df_beau <- df_beau %>%
            dplyr::filter(complete.cases(trait)) 
        names(df_beau)[1] <- trait
        return(
          df_beau %>% 
            dplyr::summarize(trait = mean(trait, na.rm = TRUE), n_taxon = dplyr::n()) %>%
              dplyr::mutate(rank = rank_name) %>%
              dplyr::mutate(name_matched = valid_name) %>%
              dplyr::mutate(source = "Bray" )
        )
      
      }
      }
      
      
      i <- i + 1
      if (i > length(candidates))
          return(dplyr::tibble(trait = -9999,
                 n_taxon = NA, rank = NA, name_matched = NA, source = "Unable to find any matches in WORMS"))
      
      }
}

find_dm_wm_ratio <- function(x) {
  df <- find_trait_explorer(x, trait = "dm_wm_ratio")
  return(df)
}

find_c_wm_ratio <- function(x) {
  df <- find_trait_explorer(x, trait = "c_wm_ratio")
  return(df)
}


# x <- "Abludomelita obtusata"
find_c_wm_ratio("Polynoinae")





