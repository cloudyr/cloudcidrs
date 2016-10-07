#' Tools to Obtain and Work with Cloud Provider CIDR Blocks
#'
#' Some cloud providers provide either an API or a file that contains all of the
#' public networks that make up their cloud infrastructure. Many force you to obtain this
#' data from publicly available internet routing registration data. Tools are provided that
#' provide a standard API to obtain the network information for supported cloud providers.
#' Each provider function returns processed, raw data structures that can be normalized
#' with additional functions to enable predictable and consistent data formats for
#' further processing.
#'
#' @name cloudcidrs
#' @docType package
#' @author Bob Rudis (bob@@rud.is)
#' @import purrr httr gdns stringi
#' @importFrom xml2 read_xml xml_find_first xml_attr xml_find_all
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom rvest html_nodes html_table
NULL

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @export
NULL
