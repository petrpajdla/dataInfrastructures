# data: get ISAD geodatabase and database

source(here::here("analysis/code/utils.r"))
source(here::here("analysis/code/get_isad.r"))


# get ISAD geodatabase -----------------------------------------------------

# UAN I ----
get_isad_geodata(return = "count", uan = "I", insecure = TRUE)
uan_i <- get_sas(return = "sas", uan = "I", insecure = TRUE)

# UAN II ----
get_isad_geodata(return = "count", uan = "IIa", insecure = TRUE)
uan_ii <- get_sas(return = "sas", uan = "IIa", insecure = TRUE)

# UAN II - zones ----
get_isad_geodata(return = "count", uan = "IIb", insecure = TRUE)
uan_iib <- get_sas(return = "sas", uan = "IIb", insecure = TRUE)

# UAN IV ----
get_isad_geodata(return = "count", uan = "IV", insecure = TRUE)
uan_iv <- get_sas(return = "sas", uan = "IV", insecure = TRUE)


# join UANs ---------------------------------------------------------------

sas_gdb <- uan_i |>
  rbind(uan_ii) |>
  rbind(uan_iib) |>
  rbind(uan_iv) |>
  janitor::clean_names()


# construct URLs ----------------------------------------------------------

# construct urls to get data from the database (https://isad.npu.cz/)
# desired format:
# https://isad.npu.cz/ + <nazev> + <id> separated by n-dashes -
# to lower > asciify > spaces to dashes > rm quotation marks > rm parentheses > add id
# if uan = ii -> only id in url
# if uan = i and is.na(nazev) - > "bez-nazvu-<id>"

isad <- "https://isad.npu.cz/"

sas_gdb <- sas_gdb |>
  dplyr::mutate(
    nazev = dplyr::if_else(nazev == ""| nazev == " ", NA_character_, nazev),
    x = tolower(nazev),
    # rm accents etc. = asciffy
    x = stringi::stri_trans_general(x, id = "Latin-ascii"),
    # insert dashes between words
    x = stringr::str_replace_all(x, "(\\s-\\s)|(,\\s)|(\\.)", "-"),
    x = stringr::str_replace_all(x, "/", "-"),
    x = stringr::str_replace_all(x, "°", "-"),
    x = stringr::str_replace_all(x, "\\s", "-"),
    # rm special characters
    x = stringr::str_remove_all(x, '\\+|"|,|´|;|ˇ|:|¨|_|=|\\?'),
    # rm parentheses
    x = stringr::str_remove_all(x, "\\(|\\)"),
    x = stringr::str_replace_all(x, "-+", "-"),
    # rm dash from beginning or end
    x = stringr::str_remove(x, "^-|-$"),
    # add id
    url = dplyr::if_else(
      is.na(x) & kategorie == "ÚAN II - pásmo", as.character(id_sas), NA_character_),
    url = dplyr::if_else(
      is.na(x) & kategorie != "ÚAN II - pásmo", paste0("bez-nazvu-", as.character(id_sas)), url),
    url = dplyr::if_else(is.na(url), paste0(x, "-", id_sas), url),
    # add prefix
    url = paste0(isad, url)) |>
  dplyr::select(-x)


# write/read geodatabase --------------------------------------------------

write_sf_gpkg(sas_gdb, here::here("analysis/data/raw_data/sas/"), "sas_gdb")
sas_gdb <- sf::st_read(here::here("analysis/data/raw_data/sas/sas_gdb_2023-12-16.gpkg"))


# get ISAD attributes -----------------------------------------------------

# # UAN IV ----
urls_uan_iv <- dplyr::filter(sas_gdb, kategorie == "ÚAN IV")$url
sas_uan_iv <- get_isad_data(urls_uan_iv)

# # UAN II - zones ----
urls_uan_iib <- dplyr::filter(sas_gdb, kategorie == "ÚAN II - pásmo")$url
sas_uan_iib <- get_isad_data(urls_uan_iib)

# UAN II ----
urls_uan_ii <- dplyr::filter(sas_gdb, kategorie == "ÚAN II")$url
sas_uan_ii <- get_isad_data(urls_uan_ii)

# UAN I ----
urls_uan_i <- dplyr::filter(sas_gdb, kategorie == "ÚAN I")$url
sas_uan_i <- get_isad_data(urls_uan_i)


# join UANs ---------------------------------------------------------------

sas_dtb <- sas_uan_i$data |>
  dplyr::bind_rows(sas_uan_ii$data) |>
  dplyr::bind_rows(sas_uan_iib$data) |>
  dplyr::bind_rows(sas_uan_iv$data)


# write database ----------------------------------------------------------

sas_dtb |> readr::write_csv(here::here("analysis/data/raw_data/sas/sas_dtb_2023-12-17.csv"))

# readr::read_csv(here::here("analysis/data/raw_data/sas/sas_dtb_2023-12-17.csv"))

