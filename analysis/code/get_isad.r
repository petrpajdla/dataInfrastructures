# get ISAD geodata

# functions ---------------------------------------------------------------

#' Get ISAD geodata
#'
#' Downloads data from ISAD ArcGIS REST API.
#'
#' @param return One of sas, count, ids, or obj. See Details.
#' @param id Object identifier to get data for.
#' @param n Size of interval.
#' @param insecure Use insecure connection, i.e. do not verify peers SSL certificate, defaults to FALSE.
#' @param uan Type of UAN. I = confirmed, IIa = presumed, IIb = zone, IV = mined area.
#' @param sleep Throttling. Lengh of sleep between cycles in a loop.
#'
#' @return
#' @export
#'
#' @examples
get_isad_geodata <- function(return = "sas", uan = "I", id, n = 1e3, insecure = FALSE, sleep = 0.2) {
  message(paste0("UANs of type ", uan, " selected."))
  stopifnot(return %in% c("sas", "count", "ids", "obj"))
  stopifnot(uan %in% c("I", "IIa", "IIb", "IV"))

  uanmap <- c("I" = 0, "IIa" = 1, "IIb" = 2, "IV" = 3)

  # url of the resource
  url <- "https://geoportal.npu.cz/arcgis/rest/services/Tematicke/CP_UAN_FS/FeatureServer/"

  # init GET request
  req0 <- httr2::request(url) |>
    httr2::req_url_path_append(
      paste0(uanmap[uan], "/", "query?"))

  # insecure connection in curl
  if (insecure == TRUE) {
    message("Using insecure connection. Authenticity of peer's SSL certificate not verified.")
    req0 <- req0 |>
      httr2::req_options(ssl_verifypeer = 0)
  }

  # count of SAS objects
  req_count <- function(req) {
    q <- req |>
      httr2::req_url_query(
        where = "1=1",
        returnCountOnly = "true",
        f = "json")

    return(q)
  }

  # ids of SAS objects
  req_ids <- function(req) {
    q <- req |>
      httr2::req_url_query(
        where = "1=1",
        returnIdsOnly = "true",
        f = "json")

    return(q)
  }

  # objects
  req_obj <- function(req, x) {
    stopifnot(is.atomic(x))
    stopifnot(is.integer(x))
    stopifnot(length(x) == 2 | length(x) == 1)

    xmin <- min(x)
    xmax <- max(x)

    q <- req |>
      httr2::req_url_query(
        where = paste0("ID_SAS>=", xmin, " AND ",
                       "ID_SAS<=", xmax),
        returnGeometry = "true",
        featureEncoding = "esriDefault",
        geometryType = "esriGeometryEnvelope",
        units = "esriSRUnit_Meter",
        outFields = "*",
        f = "geojson")

    return(q)
  }

  # perform GET query
  if (return == "count") {
    q <- req0 |>
      req_count()
    message(paste0("Sending GET request: ", q$url))
    res <- q |> httr2::req_perform() |>
      httr2::resp_check_status()|>
      httr2::resp_body_json() |>
      unlist()
  } else if (return == "ids") {
    q <- req0 |>
      req_ids()
    message(paste0("Sending GET request: ", q$url))
    resp <- q |> httr2::req_perform() |>
      httr2::resp_check_status() |>
      httr2::resp_body_json()
    res <- resp[["objectIds"]] |>
      unlist()
  } else if (return == "obj") {
    q <- req0 |>
      req_obj(x = id)
    message(paste0("Sending GET request: ", q$url))
    res <- q |> httr2::req_perform() |>
      httr2::resp_check_status() |>
      httr2::resp_body_string() |>
      geojsonsf::geojson_sf()
  } else if (return == "sas") {
    # get IDs
    ids <- req0 |>
      req_ids() |>
      httr2::req_perform() |>
      httr2::resp_check_status() |>
      httr2::resp_body_json()
    ids <- ids[["objectIds"]] |>
      unlist() |>
      as.integer() |>
      sort()

    # split IDs into several parts
    ints <- split(ids, ceiling(seq_along(ids)/n))
    imin <- lapply(ints, min)
    imax <- lapply(ints, max)

    tmp <- vector("list", length(ints))
    pb <- txtProgressBar(min = 0, max = length(tmp), initial = 0, style = 3)

    # loop through parts and parse objects as geojson
    for (i in seq_along(tmp)) {
      tmp[[i]] <- req0 |>
        req_obj(x = c(imin[[i]], imax[[i]])) |>
        httr2::req_perform() |>
        httr2::resp_check_status() |>
        httr2::resp_body_string() |>
        geojsonsf::geojson_sf()

      setTxtProgressBar(pb, i)
      Sys.sleep(sleep)
    }

    res <- do.call(rbind, tmp)
    close(pb)
  }

  return(res)
}


#' Get ISAD data/attributes
#'
#' Scrape ISAD attributes from ISAD/SAS website.
#'
#' @param urls A vector of URLs with permalinks to ISAD. Returned by
#'     \code{get_isad_geodata()} function.
#' @param sleep Throttling. Lengh of sleep between cycles in a loop.
#'
#' @return
#' @export
#'
#' @examples
get_isad_data <- function(urls, sleep = 1) {
  # output df and failed urls vector
  res <- vector("list")
  failed <- vector("character")
  # progress bar
  pb = txtProgressBar(min = 0, max = length(urls), initial = 0, style = 3)

  # extracts HTML elements as text using XPath selector
  extract_elements <- function(x, xpath) {
    x |>
      rvest::html_elements(xpath = xpath) |>
      rvest::html_text2()
  }

  # XPath selectors for SAS website
  xpths <- list(
    id_sas = "//dt[text()='ID SAS']/following-sibling::dd[1]",
    kategorie = "//dt[text()='KATEGORIE']/following-sibling::dd[1]",
    vyzn_lok = "//dt[contains(text(),'VÝZNAMNÁ')]/following-sibling::dd[1]",
    porcsas = "//dt[contains(text(),'Poř')]/following-sibling::dd[1]",
    kraj = "//dt[text()='KRAJ']/following-sibling::dd[1]",
    okres = "//dt[text()='OKRES']/following-sibling::dd[1]",
    orp = "//dt[contains(text(),'ORP')]/following-sibling::dd[1]",
    obec = "//dt[text()='OBEC']/following-sibling::dd[1]",
    katastr = "//dt[contains(text(),'KATASTR')]/following-sibling::dd[1]",
    typ = "//dt[text()='TYP']/following-sibling::dd[1]",
    lok = "//dt[contains(text(),'LOKALIZACE')]/following-sibling::dd[1]",
    obd = "//dt[contains(text(),'OBD')]/following-sibling::dd[1]",
    popis = "//h4[text()='POPIS']/following-sibling::p[1]",
    podnet = "//dt[contains(text(),'PODNĚT')]/following-sibling::dd[1]",
    presnost = "//dt[contains(text(),'PŘESNO')]/following-sibling::dd[1]",
    presnost_pozn = "//h4[contains(text(),'POZN')]/following-sibling::p",
    datum = "//dt[contains(text(),'DATUM')]/following-sibling::dd[1]",
    zdroje = "//h2[contains(text(),'SEZNAM')]/following-sibling::div/li",
    autor = "//h2[contains(text(),'AUTORIZACE')]/following-sibling::div/ul/li",
    npuuop = "//h2[contains(text(),'ÚZEMNÍ')]/following-sibling::div/ul/li")

  # loop through urls
  for (url in urls) {
    setTxtProgressBar(pb, which(urls == url))

    # GET request
    resp <- httr2::request(url) |>
      httr2::req_timeout(6e2) |>
      httr2::req_perform()

    # check request/response status (code 200)
    if (httr2::resp_status(resp) == 200) {
      # parse HTML content
      pagebody <- httr2::resp_body_html(resp)

      # extract data using XPath selectors
      res[[url]] <- lapply(xpths, \(x) extract_elements(pagebody, x)) |>
        # missing elements to NA
        lapply(\(x) if(identical(x, character(0))) NA_character_ else x) |>
        lapply(\(x) if(identical(x, "")) NA_character_ else x) |>
        # multiple values to string, sep by ;
        lapply(\(x) if(length(x) > 1) paste0(x, collapse = "; ") else x) |>
        as.data.frame()
    } else {
      # store failed URLs
      failed <- c(failed, url)
    }

    # throttle requests
    Sys.sleep(sleep)
  }

  # close progress bar
  close(pb)

  # report on failed URLs
  if (length(failed) > 0) {
    message(paste0("Some URLs have failed to fetch: ", length(failed)))
  }

  # return results
  if (length(res) > 0) {
    # convert the list to a data frame
    output <- do.call(rbind, res)
    output$url <- names(res)
  } else {
    message("No data to save...")
  }

  return(list(data = output, failed = failed))
}

