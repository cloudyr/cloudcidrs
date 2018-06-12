#' Linode ranges
#'
#' Linode (cowardly) doesn't publish their ranges, so we have to approximate with
#' known Linode ASNs
#'
#' It is unlikely that this list will change in your analysis session, so it is
#' recommended that you cache the results. Future versions will automatically cache
#' the results both in-session and on-disk for a period of time.
#'
#' @note ASN list current as of April, 2017
#' @references \url{http://bgp.he.net/search?search\%5Bsearch\%5D=\%linode\%22&commit=Search}
#' @return character vector of IPv4 ranges
#' @export
#' @examples
#' ranges <- linode_ranges()
#'
#' normalize_ipv4(ranges)
linode_ranges <- function() {

  linodes <- dplyr::filter(asn_df, stri_detect_regex(
    stri_trans_tolower(autonomous_system_organization),
    "linode")
  )
  linodes <- dplyr::pull(linodes, network)
  linodes <- discard(linodes, stri_detect_fixed, ":")

  class(linodes) <- c("cidr", "linode", class(linodes))
  linodes

}