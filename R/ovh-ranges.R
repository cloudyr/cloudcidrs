#' OVH ranges
#'
#' OVH (cowardly) doesn't publish their ranges, so we have to approximate with
#' known OVH ASNs
#'
#' It is unlikely that this list will change in your analysis session, so it is
#' recommended that you cache the results. Future versions will automatically cache
#' the results both in-session and on-disk for a period of time.
#'
#' @note ASN list current as of April, 2017
#' @return character vector of IPv4 ranges
#' @export
#' @examples
#' ranges <- ovh_ranges()
#'
#' normalize_ipv4(ranges)
ovh_ranges <- function() {

  ovh <- dplyr::filter(asn_df, stri_detect_regex(
    stri_trans_tolower(autonomous_system_organization),
    "ovh")
  )
  ovh <- dplyr::pull(ovh, network)
  ovh <- discard(ovh, stri_detect_fixed, ":")

  class(ovh) <- c("cidr", "ovh", class(ovh))
  ovh

}