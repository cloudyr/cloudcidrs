#' Amazon AWS cloud ranges
#'
#' Retrieves the official list of Amazon cloud network ranges.
#'
#' Amazon provides their official netblock list in \href{https://ip-ranges.amazonaws.com/ip-ranges.json}{JSON format}.
#'
#' It is unlikely that this list will change in your analysis session, so it is
#' recommended that you cache the results. Future version will automatically cache
#' the results both in-session and on-disk for a period of time.
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

#' Rackspace ranges
#'
#' Rackspace (cowardly) doesn't publish their ranges, so we have to approximate with
#' known Rackspace Hosting ASNs
#'
#' It is unlikely that this list will change in your analysis session, so it is
#' recommended that you cache the results. Future version will automatically cache
#' the results both in-session and on-disk for a period of time.
#'
#' @note ASN list current as of October, 2016
#' @references \url{http://bgp.he.net/search?search\%5Bsearch\%5D=\%22Rackspace\%22&commit=Search}
#' @return character vector of IPv4 ranges
#' @export
#' @examples
#' ranges <- rackspace_ranges()
#'
#' normalize_ipv4(ranges)
rackspace_ranges <- function() {

  rs_asns <- c("AS58683", "AS54636", "AS45187", "AS44716", "AS39921", "AS36248",
               "AS33070", "AS27357", "AS22720", "AS19994", "AS15395", "AS12200",
               "AS10532")

  purrr::map(sprintf("http://ipinfo.io/%s", rs_asns), function(rs_asn_url) {
    res <- httr::GET(rs_asn_url)
    r <- httr::content(res, as="parsed")
    rvest::html_nodes(r, xpath=".//h2[@id='blocks']/following-sibling::table[1]") %>%
      rvest::html_table() -> tab
    if (!("Netblock" %in% colnames(tab[[1]]))) return(NULL)
    unlist(tab[[1]]$Netblock)
  }) %>%
    purrr::discard(is.null) %>%
    purrr::flatten_chr() -> racks

  class(racks) <- c("cidr", "rackspace", class(racks))
  racks

}

#' Digital Ocean ranges
#'
#' Digitial Ocean (cowardly) doesn't publish their ranges, so we have to approximate with
#' known Digital Ocean ASNs.
#'
#' It is unlikely that this list will change in your analysis session, so it is
#' recommended that you cache the results. Future version will automatically cache
#' the results both in-session and on-disk for a period of time.
#'
#' @note ASN list current as of October, 2016
#' @references \url{http://bgp.he.net/search?search\%5Bsearch\%5D=\%22Digital+Ocean\%22&commit=Search}
#' @return character vector of IPv4 ranges
#' @export
#' @examples
#' ranges <- digitalocean_ranges()
#'
#' normalize_ipv4(ranges)
digitalocean_ranges <- function() {

  do_asns <- c("AS62567", "AS394362", "AS393406", "AS202109", "AS202018",
               "AS201229", "AS200130", "AS14061", "AS135340", "AS133165")

  purrr::map(sprintf("http://ipinfo.io/%s", do_asns), function(do_asn_url) {
    res <- httr::GET(do_asn_url)
    r <- httr::content(res, as="parsed")
    rvest::html_nodes(r, xpath=".//h2[@id='blocks']/following-sibling::table[1]") %>%
      rvest::html_table() -> tab
    if (!("Netblock" %in% colnames(tab[[1]]))) return(NULL)
    unlist(tab[[1]]$Netblock)
  }) %>%
    purrr::discard(is.null) %>%
    purrr::flatten_chr() -> h2o

  class(h2o) <- c("cidr", "digitalocean", class(h2o))

  h2o

}

#' Google Cloud ranges
#'
#' Retrieves the official list of Google cloud network ranges.
#'
#' Google publishes their Compute Engine IP blocks via
#' \href{https://cloud.google.com/compute/docs/faq#where_can_i_find_short_product_name_ip_ranges}{DNS TXT records}.
#' While accurate, this list may not be complete as Google scales their public cloud
#' infrastructure to meet demand and owns a large number of netblocks.
#'
#' It is unlikely that this list will change in your analysis session, so it is
#' recommended that you cache the results. Future version will automatically cache
#' the results both in-session and on-disk for a period of time.
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
#' Retrieves the official list of Azure cloud network ranges.
#'
#' Microsoft publishes their Azure ranges via a downloadable XML (ugh) file that you
#' can retrieve starting from
#' \href{https://www.microsoft.com/en-us/download/confirmation.aspx?id=41653}{this page}.
#'
#' It is unlikely that this list will change in your analysis session, so it is
#' recommended that you cache the results. Future version will automatically cache
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
        as_tibble()
    }) -> azur

  class(azur) <- c("cidr", "azure", class(azur))
  azur

}

#' Softlayer ranges
#'
#' Retrieves the official list of Softlayer cloud network ranges.
#'
#' Softlayer provides \href{https://knowledgelayer.softlayer.com/content/what-ip-ranges-do-i-allow-through-firewall}{a list}
#' of public netblock ranges. Softlayer also has a large number of large ASNs. Which
#' list is accurate is up for debate. Methods are provided that enable using either of
#' these sources to generate the CIDR list.
#'
#' It is unlikely that this list will change in your analysis session, so it is
#' recommended that you cache the results. Future version will automatically cache
#' the results both in-session and on-disk for a period of time.
#'
#' @param method if \code{list}, this method will use the HTML published ranges; if
#'        \code{asn}, this method will build the CIDR list from Softlayer published
#'        ASNs.
#' @return a \code{tibble}, the most interesting colun of  which is \code{ip_range}
#' @export
#' @examples
#' ranges <- softlayer_ranges()
#'
#' normalize_ipv4(ranges)
softlayer_ranges <- function(method="list") {

  method <- match.arg(method, c("list", "asn"))

  if (method=="list") {

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

  } else if (method=="asn") {

    sl_asns <- c("AS46704", "AS46703", "AS46702", "AS36420", "AS36351",
                 "AS30315", "AS21844", "AS13884", "AS13749")

    purrr::map(sprintf("http://ipinfo.io/%s", sl_asns), function(sl_asn_url) {
      res <- httr::GET(sl_asn_url)
      r <- httr::content(res, as="parsed")
      rvest::html_nodes(r, xpath=".//h2[@id='blocks']/following-sibling::table[1]") %>%
        rvest::html_table() -> tab
      if (!("Netblock" %in% colnames(tab[[1]]))) return(NULL)
      unlist(tab[[1]]$Netblock)
    }) %>%
      purrr::discard(is.null) %>%
      purrr::flatten_chr() -> soft
  }

  soft

}