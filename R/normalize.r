#' Normalize Cloud CIDR return values
#'
#' Calling the cloud ranges functions directly returns raw data from the
#' various platform responses. This function will return a flat character vector
#' of IPv4 CIDR blocks from a cloud provider response so it can be more easily used
#' in downstream analysis operations
#'
#' @param x the output of one of the cloud \code{_ranges} functions
#' @export
#' @examples
#' amazon_ranges() %>% normalize_ipv4()
#' azure_ranges() %>% normalize_ipv4()
#' digitalocean_ranges() %>% normalize_ipv4()
#' google_ranges() %>% normalize_ipv4()
#' racksapce_ranges() %>% normalize_ipv4()
#' softlayer_ranges() %>% normalize_ipv4()
normalize_ipv4 <- function(x) {

  if (inherits(x, "amazon")) return(unlist(x$prefixes$ip_prefix))
  if (inherits(x, "azure")) return(unlist(x$subnets))
  if (inherits(x, "digitalocean")) return(x)
  if (inherits(x, "google")) return(unlist(x$ipv4))
  if (inherits(x, "rackspace")) return(x)
  if (inherits(x, "softlayer")) return(unlist(x$ip_range))

  warning("Cloud provider not recognized")
  return(NULL)

}