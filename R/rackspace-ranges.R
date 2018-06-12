#' Rackspace ranges
#'
#' Rackspace (cowardly) doesn't publish their ranges, so we have to approximate with
#' known Rackspace Hosting ASNs
#'
#' It is unlikely that this list will change in your analysis session, so it is
#' recommended that you cache the results. Future versions will automatically cache
#' the results both in-session and on-disk for a period of time.
#'
#' @note ASN list current as of June 2018
#' @return character vector of IPv4 ranges
#' @export
#' @examples
#' ranges <- rackspace_ranges()
#'
#' normalize_ipv4(ranges)
rackspace_ranges <- function() {

  racks <- dplyr::filter(asn_df, stri_detect_regex(
    stri_trans_tolower(autonomous_system_organization),
    "rackspace")
  )
  racks <- dplyr::pull(racks, network)
  racks <- discard(racks, stri_detect_fixed, ":")

  class(racks) <- c("cidr", "rackspace", class(racks))
  racks

}