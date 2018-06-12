sl_md <- "https://raw.githubusercontent.com/IBM-Bluemix-Docs/hardware-firewall-dedicated/master/ips.md"

#' Softlayer ranges
#'
#' Retrieves the official list of Softlayer cloud network ranges.
#'
#' Softlayer provides \href{https://github.com/IBM-Bluemix-Docs/hardware-firewall-dedicated/blob/master/ips.md}{a list}
#' of public netblock ranges. Softlayer also has a large number of large ASNs but
#' Softlayer tech support claimes AS36351 is their "public cloud" ASN.
#' Methods are provided that enable using either of these sources to generate the CIDR list.
#'
#' It is unlikely that this list will change in your analysis session, so it is
#' recommended that you cache the results. Future versions will automatically cache
#' the results both in-session and on-disk for a period of time.
#'
#' @param method if \code{list}, this method will use the HTML published ranges; if
#'        \code{asn}, this method will build the CIDR list from Softlayer published
#'        ASNs. The default method is "\code{asn}".
#' @return a \code{tibble}, the most interesting colun of  which is \code{ip_range}
#' @export
#' @examples
#' ranges <- softlayer_ranges()
#'
#' normalize_ipv4(ranges)
softlayer_ranges <- function(method=c("asn", "list")) {

  method <- match.arg(method, c("asn", "list"))

  if (method=="list") {

    tfmd <- tempfile(fileext = ".md")
    on.exit(unlink(tfmd), add=TRUE)

    tfh <- tempfile(fileext = ".html")
    on.exit(unlink(tfh), add=TRUE)

    download.file(sl_md, tfmd)

    outf <- suppressMessages(knitr::pandoc(tfmd, tfh, format = "html"))
    on.exit(unlink(tfmd), add=TRUE)

    xml2::read_html(outf) %>%
      rvest::html_table() %>%
      lapply(`[[`, "IP Range") %>%
      Filter(Negate(is.null), .) %>%
      unlist() %>%
      strsplit(" and |, | +") %>%
      unlist() %>%
      Filter(function(x) { !grepl("^10\\.|:", x)}, .)

    class(soft) <- c("cidr", "softlayer", class(soft))

    soft

  } else if (method=="asn") {

    sl_asns <- c("AS36351")
    # sl_asns <- c("AS46704", "AS46703", "AS46702", "AS36420", "AS36351",
    #              "AS30315", "AS21844", "AS13884", "AS13749")

    soft <- dplyr::filter(asn_df, stri_detect_regex(
      stri_trans_tolower(autonomous_system_organization),
      "softlayer")
    )
    soft <- dplyr::pull(soft, network)
    soft <- discard(soft, stri_detect_fixed, ":")

    class(soft) <- c("cidr", "softlayer", class(soft))

    soft

  }

}