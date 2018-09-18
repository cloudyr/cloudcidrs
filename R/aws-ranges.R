#' Amazon AWS cloud ranges
#'
#' Retrieves the official list of Amazon cloud network ranges.
#'
#' Amazon provides their official netblock list in \href{JSON format}{https://ip-ranges.amazonaws.com/ip-ranges.json}.
#'
#' It is unlikely that this list will change in your analysis session, so it is
#' recommended that you cache the results. Future versions will automatically cache
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

  res$prefixes <- dplyr::tbl_df(res$prefixes)
  res$ipv6_prefixes <- dplyr::tbl_df(res$ipv6_prefixes)

  class(res) <- c("cidr", "amazon", class(res))
  res

}
