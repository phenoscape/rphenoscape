# Extract status code from response and status handling

stop_for_pk_status <- function(x) {
  status <- x$status_code
  if (x$status_code < 300) {
    return(invisible(TRUE))
  }

  if (x$status_code == 500 ) {
    stop(httr::http_condition(x, "error", message = "Malformed parameters.", call = sys.call(-1)))
  }

  if (x$status_code == 404) {
    stop(httr::http_condition(x, "error", message = "URL path used for this version is outdated, please update.", call = sys.call(-1)))
  }

  if (x$status_code == 504) {
    stop(httr::http_condition(x, "error", message = "The search term is too generic; server timed out.", call = sys.call(-1)))
  }

  stop(httr::http_condition(x, "error", call=sys.call(-1)))

}

