
# read from package data
# read from package data
# use package data instead



find_trait_explorer <- function(x, trait = ""){
  # some error handling
  
  df_traits <- readr::read_rds(system.file("extdata", "bray_ratios.rds", package = "traitfinder"))
  
  traits <- df_traits %>% 
    dplyr::select(-class, -phylum, -order, -family, -genus, -species) %>% 
    names()
  
  # throw error if trait is not in traits
  # This should list the traits available, using traits
  # traits split out, i.e trait1, trait2,
  trait_list <- stringr::str_c(traits, collapse = ", ")
  if (!(trait %in% traits))
    return(stringr::str_glue("Trait {trait} not found. Available traits are: {trait_list}"))
  
  # check trait is valid
  
  df_traits <- df_traits %>% 
    # select trait
    dplyr::select(dplyr::one_of(trait), class, phylum, order, family, genus, species) %>% 
    tidyr::drop_na()
  
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
      
      rank_name <- tolower(df_worms$rank)
      if (rank_name == "phylum (division)")
        rank_name = "phylum"
      
      
      if (rank_name == "Kingdom")
          return(dplyr::tibble(trait = -9999,
                 n_taxon = NA, rank = NA, name_matched = NA, source = "Unable to find any matches in WORMS"))
      
      candidates <- dplyr::select(df_worms, genus, family, order, class, phylum)
      candidates <- tidyr::gather(candidates, key, value, phylum:genus) 
      candidates <- dplyr::filter(candidates, !stringr::str_detect(value, "unassigned"))
      candidates <- candidates %>% 
        tidyr::spread(key, value)
      candidates <- dplyr::select(candidates, dplyr::contains("genus"), dplyr::contains("family"), dplyr::contains("order"), dplyr::contains("class"), dplyr::contains("phylum"))
      
      df_worms <- dplyr::select(df_worms, valid_name)
      
      names(df_worms) <- rank_name
      
      i <- 1
      while (TRUE){
      
      bad <- FALSE
        if (i == 1){
      df_worms <- dplyr::slice(df_worms, 1)
      
       if ((stringr::str_to_lower(rank_name) %in% c("suborder", "subphylum", "parvphylum", "subfamily", "subspecies", "kingdom", "superfamily",  "infraorder")) == TRUE)
         bad <- TRUE
       x_value <- df_worms %>% 
         dplyr::select(1) %>% 
         dplyr::slice(1) %>% 
         dplyr::pull()
       if ((stringr::str_to_lower(rank_name) %in% c("suborder", "subphylum", "parvphylum", "subfamily", "subspecies", "kingdom", "superfamily", "infraorder")) == FALSE  & complete.cases(x_value) ){
      
      if ("subclass" %in% names(df_worms))
        df_worms <- dplyr::rename(df_worms, Order = subclass) 
      
      # make all column names lower case
      df_worms <- dplyr::rename_all(df_worms, tolower)
       }
        }
      if (i > 1){
        df_worms <- candidates %>% 
        dplyr::select(i-1)
        rank_name <- names(df_worms)[1]
      }
      
      if (bad == FALSE & complete.cases(x_value)){
      suppressMessages(df_beau <- df_worms %>% 
        dplyr::select_if(~ !any(is.na(.))) %>% 
        dplyr::inner_join(df_traits) )
      if (nrow(df_beau) > 0) {
        # pull the first column
        valid_name <- df_beau %>% 
          dplyr::select(1) %>% 
          dplyr::slice(1) %>% 
          dplyr::pull(.)
        df_beau <- df_beau %>%
            dplyr::filter(complete.cases(trait)) 
        df_out <- df_beau %>% 
            dplyr::summarize(trait = mean(trait, na.rm = TRUE), n_taxon = dplyr::n()) %>%
              dplyr::mutate(rank = rank_name) %>%
              dplyr::mutate(name_matched = valid_name) %>%
              dplyr::mutate(source = "Bray" )
        names(df_out)[1] <- trait
        return(
          df_out
        )
      
      }
      }
      
      
      i <- i + 1
      if (i > length(candidates))
          return(dplyr::tibble(trait = -9999,
                 n_taxon = NA, rank = NA, name_matched = NA, source = "Unable to find any matches in WORMS"))
      
      }
}

find_ratio <- function(x, ratio = "") {
  
  df <- find_trait_explorer(x, trait = ratio)
  names(df)[1] <- ratio 
  
  return(df)
}
