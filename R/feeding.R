

find_feeding <- function(x){
  # some manual stuff
  
  if(x == "Aplacophora")
    return(tibble(suspension = 1, n_taxon = 1, rank = "class", name_matched = "Aplacophora", source = "Manual"))
  # read from package data
  df_cefas <- readr::read_rds(system.file("extdata", "cefas_feeding.rds", package = "traitfinder"))
  
  # read from package data
  df_traits <- readr::read_rds(system.file("extdata", "beauchard_2023.rds", package = "traitfinder"))
  ###
  # function to extract the feeding trait from the WORMS database
  # x is the species name
  # returns the mean suspension feeding trait
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
      
      x = traitfinder::name_match(x)
      
      # start of by seeing if we can find it based on the species info in beauchard
      df_try <- df_traits %>% 
        dplyr::filter(Species == x) 
      if(nrow(df_try) > 0){
        return(
          df_try %>% 
            dplyr::filter(complete.cases(Suspension)) %>%
            dplyr::summarize(suspension = mean(Suspension, na.rm = TRUE), n_taxon = dplyr::n()) %>%
              dplyr::mutate(rank = "species") %>% 
              dplyr::mutate(name_matched = x) %>%
              dplyr::mutate(source = "Beauchard et al. 2023" )
        )
      }
      
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
          dplyr::tibble(suspension = -9999,
                 n_taxon = NA, rank = NA, name_matched = NA, source = "Unable to find any matches in WORMS")
        )
      
      df_worms <- dplyr::slice(df_worms, 1)
      # we now need to check the worms name is not radically different....
      if(stringdist::stringsim(stringr::str_to_lower(x), stringr::str_to_lower(df_worms$scientificname[1])) < 0.7)
        return(dplyr::tibble(suspension = -9999,
                 n_taxon = NA, rank = NA, name_matched = NA, source = "Unable to find any matches in WORMS"))
    
      # check if the status was deleted
      if (df_worms$status[1] == "deleted")
          return(dplyr::tibble(suspension = -9999,
                 n_taxon = NA, rank = NA, name_matched = NA, source = "Unable to find any matches in WORMS"))
      
      rank_name <- df_worms$rank
      
      if (rank_name == "Kingdom")
          return(dplyr::tibble(suspension = -9999,
                 n_taxon = NA, rank = NA, name_matched = NA, source = "Unable to find any matches in WORMS"))
      
      candidates <- dplyr::select(df_worms, genus, family, order, class, phylum)
      candidates <- tidyr::gather(candidates, key, value, phylum:genus) 
      candidates <- dplyr::filter(candidates, !stringr::str_detect(value, "unassigned"))
      candidates <- candidates %>% 
        tidyr::spread(key, value)
      candidates <- dplyr::select(candidates, dplyr::contains("genus"), dplyr::contains("family"), dplyr::contains("order"), dplyr::contains("class"), dplyr::contains("phylum"))
      
      df_worms <- dplyr::select(df_worms, valid_name)
      
      names(df_worms) <- rank_name
      
      i <- 0
      
      while (TRUE){
      
      if (i > 0){
        df_worms <- candidates %>% 
          dplyr::select(i)
        
      }
      
      bad <- FALSE
      
      if(i > 0){
        rank_name <- df_worms %>% 
          dplyr::select(1) %>%
          names()
      }
      
      
      bad <- FALSE
      
      if ((rank_name %in% c("Suborder", "Parvphylum", "Subfamily", "Subspecies", "Kingdom", "Subclass", "Subphylum",  "Infraorder")))
        bad <- TRUE
        
      if (bad == FALSE){
        x_value <- df_worms %>% 
          dplyr::select(1) %>% 
          dplyr::slice(1) %>% 
          dplyr::pull()
      if ((rank_name %in% c("Suborder", "Parvphylum", "Subfamily", "Subspecies", "Kingdom", "Subclass")) == FALSE & complete.cases(x_value)){
      
      suppressMessages(df_beau <- df_worms %>% 
        dplyr::select_if(~ !any(is.na(.))) %>% 
        dplyr::inner_join(df_traits) )
    
      
      
      if (nrow(df_beau) > 0) {
        out_name <- df_worms %>% 
          dplyr::select(1) %>% 
          dplyr::slice(1) %>% 
          dplyr::pull() 
        
        return(
          df_beau %>% 
            dplyr::filter(complete.cases(Suspension)) %>%
            dplyr::summarize(suspension = mean(Suspension, na.rm = TRUE), n_taxon = dplyr::n()) %>%
              dplyr::mutate(rank = rank_name) %>% 
              dplyr::mutate(rank = stringr::str_to_lower(rank)) %>% 
              dplyr::mutate(name_matched = out_name) %>%
              dplyr::mutate(source = "Beauchard et al. 2023" )
        )
      }
      
      # make df_worms column names lower case
      names(df_worms) <- tolower(names(df_worms))
      if ("species" %in% names(df_worms) == FALSE){
      suppressMessages(df_trait <- df_cefas %>% 
        dplyr::inner_join(df_worms) )
      if(nrow(df_trait) > 0){
        
        out_name <- df_worms %>% 
          dplyr::select(1) %>% 
          dplyr::slice(1) %>% 
          dplyr::pull()
        
        trait <- df_trait %>% 
          dplyr::ungroup() %>% 
          # pull(Suspension) %>% 
          dplyr::filter(complete.cases(Suspension)) %>%
            dplyr::summarize(suspension = mean(Suspension, na.rm = TRUE), n_taxon = dplyr::n()) %>%
              dplyr::mutate(rank = rank_name) %>% 
              dplyr::mutate(rank = stringr::str_to_lower(rank)) %>% 
              dplyr::mutate(name_matched = out_name) %>%
              dplyr::mutate(source = "Cefas" ) 
      return(trait)
      }
      # }
      
      # }
      
      # }
      }
      }
      }
      i <- i + 1
      
      if (i > length(candidates))
          return(dplyr::tibble(suspension = -9999,
                 n_taxon = NA, rank = NA, name_matched = NA, source = "Unable to find any matches in WORMS"))
      
      }
}
