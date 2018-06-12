#' Digital Ocean ranges
#'
#' Digitial Ocean (cowardly) doesn't publish their ranges, so we have to approximate with
#' known Digital Ocean ASNs.
#'
#' It is unlikely that this list will change in your analysis session, so it is
#' recommended that you cache the results. Future versions will automatically cache
#' the results both in-session and on-disk for a period of time.
#'
#' @note ASN list current as of June 2018
#' @return character vector of IPv4 ranges
#' @export
#' @examples
#' ranges <- digitalocean_ranges()
#'
#' normalize_ipv4(ranges)
digitalocean_ranges <- function() {

  h2o <- dplyr::filter(asn_df, stri_detect_regex(
    stri_trans_tolower(autonomous_system_organization),
    "digitalocean|digital ocean")
  )
  h2o <- dplyr::pull(h2o, network)
  h2o <- discard(h2o, stri_detect_fixed, ":")

  class(h2o) <- c("cidr", "digitalocean", class(h2o))

  h2o

}
