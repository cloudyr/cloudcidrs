#' Amazon AWS cloud ranges
#'
#' @return list of 4 slots the most interesting of which is \code{prefixes} which
#'         is a \code{tibble} with three columns, the most interesting of which is
#'         \code{ip_prefix}, but the other columns are useful for various tasks. There is
#'         an IPv6 counterpart slot \code{ipv6_prefixes} with a similar sub-structure.
#' @export
#' @examples
#' ranges <- amazon_ranges()
#'
#' normalize_ipv4(ranges)
amazon_ranges <- function() {

  res <- jsonlite::fromJSON("https://ip-ranges.amazonaws.com/ip-ranges.json", flatten=TRUE)

  res$prefixes <- as_tibble(res$prefixes)
  res$ipv6_prefixes <- as_tibble(res$ipv6_prefixes)

  class(res) <- c("cidr", "amazon", class(res))
  res

}

#' Google Cloud ranges
#'
#' @return list of 2 slots, one for \code{ipv4} and one for \code{ipv6} ranges
#' @export
#' @examples
#' ranges <- google_ranges()
#'
#' normalize_ipv4(ranges)
google_ranges <- function() {

  gcloud <- gdns::bulk_query("_cloud-netblocks.googleusercontent.com", "txt")

  stri_split_fixed(gcloud$data, " ")[[1]] %>%
    purrr::keep(stri_detect_regex, "^include") %>%
    stri_replace_first_regex("^include:", "") %>%
    purrr::map(gdns::bulk_query, "txt") %>%
    purrr::map("data") %>%
    purrr::flatten_chr() %>%
    stri_split_fixed(" ") %>%
    purrr::flatten_chr() %>%
    purrr::keep(stri_detect_regex, "^ip") -> gcloud

  list(ipv4=purrr::keep(gcloud, stri_detect_regex, "^ip4") %>%
         stri_replace_first_regex("^ip4:", ""),
       ipv6=purrr::keep(gcloud, stri_detect_regex, "^ip6") %>%
         stri_replace_first_regex("^ip6:", "")) -> gcloud

  class(gcloud) <- c("cidr", "google", class(gcloud))
  gcloud

}

#' Azure ranges
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

  xml_find_first(r, ".//a[contains(@href, 'download.microsoft.com')]") %>%
    xml_attr("href") -> azure_link

  res <- httr::GET(azure_link)

  r <- httr::content(res, as="text", encoding="UTF-8")

  doc <- xml2::read_xml(r)
  xml_find_all(doc, ".//Region") %>%
    purrr::map_df(function(x) {
      region_name <- xml_attr(x, "Name")
      xml_find_all(x, ".//IpRange") %>%
        xml_attr("Subnet") -> subnets
      data.frame(region_name, subnets, stringsAsFactors=FALSE) %>%
        as_tibble()
    }) -> azur

  class(azur) <- c("cidr", "azure", class(azur))
  azur

}

#' Softlayer ranges
#'
#' @return a \code{tibble}, the most interesting colun of  which is \code{ip_range}
#' @export
#' @examples
#' ranges <- softlayer_ranges()
#'
#' normalize_ipv4(ranges)
softlayer_ranges <- function() {

  res <- httr::GET("https://knowledgelayer.softlayer.com/content/what-ip-ranges-do-i-allow-through-firewall")
  r <- httr::content(res, as="parsed", encoding="UTF-8")
  rvest::html_nodes(r, xpath=".//table[1]") %>%
    rvest::html_table() -> soft

  soft <- soft[[1]]

  colnames(soft) <-
    colnames(soft) %>%
    stri_trans_tolower() %>%
    stri_replace_all_fixed(" ", "_")

  soft <- as_tibble(soft)

  class(soft) <- c("cidr", "softlayer", class(soft))

  soft

}