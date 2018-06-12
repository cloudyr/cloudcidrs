#' Azure ranges
#'
#' Retrieves the official list of Azure cloud network ranges.
#'
#' Microsoft publishes their Azure ranges via a downloadable XML (ugh) file that you
#' can retrieve starting from
#' \href{https://www.microsoft.com/en-us/download/confirmation.aspx?id=41653}{this page}.
#'
#' It is unlikely that this list will change in your analysis session, so it is
#' recommended that you cache the results. Future versions will automatically cache
#' the results both in-session and on-disk for a period of time.
#'
#' @return \code{data.frame} with the cloud \code{region} and cidr \code{subnet}. As of
#'         2016-10-04, these subnets are IPv4 only.
#' @export
#' @examples
#' ranges <- azure_ranges()
#'
#' normalize_ipv4(ranges)
azure_ranges <- function() {

  res <- httr::GET("https://www.microsoft.com/en-us/download/confirmation.aspx?id=41653")

  r <- httr::content(res, as="parsed", encoding="UTF-8")

  xml2::xml_find_first(r, ".//a[contains(@href, 'download.microsoft.com')]") %>%
    xml2::xml_attr("href") -> azure_link

  res <- httr::GET(azure_link)

  r <- httr::content(res, as="text", encoding="UTF-8")

  doc <- xml2::read_xml(r)
  xml2::xml_find_all(doc, ".//Region") %>%
    purrr::map_df(function(x) {
      region_name <- xml_attr(x, "Name")
      xml2::xml_find_all(x, ".//IpRange") %>%
        xml2::xml_attr("Subnet") -> subnets
      data.frame(region_name, subnets, stringsAsFactors=FALSE) %>%
        dplyr::tbl_df()
    }) -> azur

  class(azur) <- c("cidr", "azure", class(azur))
  azur

}
