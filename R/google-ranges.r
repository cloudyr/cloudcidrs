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
#' recommended that you cache the results. Future versions will automatically cache
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
