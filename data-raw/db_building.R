## database building scripts
library(tidyverse)

# reduce to full_join
list.files('data-raw', pattern = '.xlsx', recursive = TRUE, full.names = TRUE) %>%
  magrittr::extract(c(1:6, 8:9, 11:17)) %>%
  purrr::map(~ readxl::read_excel(.x, na = c('NA'))) %>%
  purrr::map(~ dplyr::select(.x, -3)) %>%
  purrr::map(~ dplyr::mutate(.x, var_description = names(.x)[3])) %>%
  purrr::map(
    ~ magrittr::set_names(
      .x, c('municipality_id', 'municipality_name', 'raw_value', 'ranked_value', 'var_description')
    )
  ) %>%
  # purrr::map_dfr(
  #   ~ magrittr::set_names(
  #     .x, c('municipality_id', 'municipality_name', 'raw_value', 'ranked_value', 'var_description')
  #   )
  # ) %>%
  purrr::map(
    ~ dplyr::mutate(.x,
      var_id = dplyr::case_when(
        stringr::str_detect(var_description, 'bolets') ~ 'mushroom',
        stringr::str_detect(var_description, 'Escorrentiu') ~ 'runoff',
        stringr::str_detect(var_description, 'Fusta i') ~ 'wood',
        stringr::str_detect(var_description, 'bosc natural') ~ 'cover_natural',
        stringr::str_detect(var_description, 'bosc de ribera') ~ 'cover_riparian',
        stringr::str_detect(var_description, 'pendents') ~ 'cover_slope',
        stringr::str_detect(var_description, 'Embornal') ~ 'sink_c',
        stringr::str_detect(var_description, 'Emmagatz') ~ 'water_store',
        stringr::str_detect(var_description, 'Estoc de') ~ 'stock_c',
        stringr::str_detect(var_description, "d'animals") ~ 'animal_obs',
        stringr::str_detect(var_description, 'Wikiloc') ~ 'wikiloc',
        stringr::str_detect(var_description, 'Tursime') ~ 'turism',
        stringr::str_detect(var_description, 'Xarxa') ~ 'nw2000',
        stringr::str_detect(var_description, "d'aus") ~ 'rich_birds',
        stringr::str_detect(var_description, "llenyoses") ~ 'rich_trees'
      )
    )
  ) %>%
  purrr::map(
    ~ magrittr::set_names(.x, c(
        'municipality_id', 'municipality_name', unique(.x[['var_id']]),
        glue::glue("{unique(.x[['var_id']])}_ranked"),
        'var_description', 'var_id'
      )
    )
  ) -> all_services_list


services_var_thesaurus <- all_services_list %>%
  purrr::map_dfr(~dplyr::select(.x, var_id, var_description)) %>%
  dplyr::distinct()

all_services_list %>%
  purrr::map(~ dplyr::select(.x, -var_description, -var_id)) %>%
  purrr::reduce(
    ~ dplyr::full_join(.x, .y, by = c('municipality_id', 'municipality_name'))
  ) -> municipality_services

usethis::use_data(
  services_var_thesaurus, municipality_services,
  # plot_services,
  internal = TRUE, overwrite = TRUE
)
