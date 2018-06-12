#' Build a complete data frame of all known cloud provider ranges
#'
#' @md
#' @export
all_ranges <- function() {

  suppressWarnings(
    dplyr::bind_rows(
      dplyr::data_frame( provider = "amazon", cidr = amazon_ranges() %>% normalize_ipv4()),
      dplyr::data_frame( provider = "azure", cidr = azure_ranges() %>% normalize_ipv4()),
      dplyr::data_frame( provider = "digitalocean", cidr = digitalocean_ranges() %>% normalize_ipv4()),
      dplyr::data_frame( provider = "google", cidr = google_ranges() %>% normalize_ipv4()),
      dplyr::data_frame( provider = "linode", cidr = linode_ranges() %>% normalize_ipv4()),
      dplyr::data_frame( provider = "rackspace", cidr = rackspace_ranges() %>% normalize_ipv4()),
      dplyr::data_frame( provider = "softlayer", cidr = softlayer_ranges() %>% normalize_ipv4())
    )
  ) -> out

  out <- dplyr::bind_cols(out, iptools::range_boundaries(out$cidr))
  out <- dplyr::select(out, -range)

  out <- dplyr::mutate(out, check_date = Sys.Date())

  out

}