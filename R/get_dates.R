replace_null <- function(x) {
  x <- purrr::map(x, ~ replace(.x, is.null(.x), NA_character_))
  # x <- purrr::map(x, ~ ifelse(is.null(.x), NA, .x))
  purrr::map(x, ~ if(is.list(.x)) replace_null(.x) else .x)
}

get_dates <- function(x) {
  # query Neotoma for data set
  base.uri <- 'http://api.neotomadb.org/v2.0/data/downloads'

  neotoma_content <- httr::content(httr::GET(paste0(base.uri, '/', x)),
                                   as = "text")
  if (identical(neotoma_content, "")) stop("")
  result <- jsonlite::fromJSON(neotoma_content, simplifyVector = FALSE)

  parse_chronology <- function(chrono) {
    suppressMessages({
      ID <- chrono$chronologyid
      chronology <- chrono %>%
        purrr::pluck("chronology") %>%
        tibble::as_tibble() %>%
        # tidyr::unnest(contact) %>%
        dplyr::mutate(chronologyid = ID, .before = 1) %>%
        readr::type_convert()
      chroncontrols <- chrono %>%
        purrr::pluck("chroncontrols") %>%
        purrr::map_df(parse_date) %>%
        dplyr::mutate(chronologyid = ID, .before = 1) %>%
        readr::type_convert()
    })
  }

  parse_date <- function(date) {
    date$geochron <- date$geochron %>%
      tibble::as_tibble()
    tibble::as_tibble(date) %>%
      tidyr::unnest(geochron)
  }

  tryCatch({
    collectionunit <- result %>%
      purrr::pluck("data") %>%
      .[[1]] %>%
      purrr::pluck("site") %>%
      purrr::pluck("collectionunit") %>%
      replace_null()
    default_chron_id <- getElement(collectionunit, "defaultchronology")
    tmp <- collectionunit %>%
      purrr::pluck("chronologies") %>%
      purrr::flatten() %>%
      purrr::map_df(parse_chronology) %>%
      dplyr::select(chronology.id = chronologyid,
                    lab.no = labnumber,
                    geo.chron.type = geochrontype,
                    material.dated = geochronmaterialdated,
                    radiocarbon.method = radiocarbonmethod,
                    age = geochronage,
                    e.older = geochronerrorolder,
                    e.young = geochronerroryounger,
                    depth,
                    thickness,
                    notes = geochronnotes,
                    infinite,
                    delta13C = delta13c,
                    age.type = agetype) %>%
      dplyr::arrange(geo.chron.type, age) %>%
      dplyr::mutate(dataset.id = collectionunit$dataset$datasetid,
                    handle = collectionunit$handle,
                    .before = 1) %>%
      dplyr::filter(!is.na(age)) %>%
      dplyr::mutate(dataset.id = as.integer(dataset.id),
                    lab.no = as.character(lab.no),
                    notes = as.character(notes))
    if (!is.null(default_chron_id) && !is.na(default_chron_id))
      tmp <- dplyr::filter(tmp, chronology.id %in% default_chron_id)
    tmp
  },
  error = function(e) NULL)
}
