# script : bibliometrics.R
# author : Benjamin Rubinstein
# about  : extracts citation counts from a Google Scholar profile.
#          RefManageR should do this with ReadGS() but doesn't work.

library(httr)
library(xml2)
library(RefManageR)
library(stringr)

# Collects up to 100 most-cited publications from a Google Scholar
# profile, extracting authorYearTitle keys and citation counts. Given
# - Scholar ID
# - Encoding 
# - gs: previous raw download, useful for development - not wanting to hit rate limits
# Returns a list with slots
# - pubs:   vector of citations counts, named by keys
# - cites:  total citations
# - hindex: h-index
# - gs:     the raw search results, useful for development
ScholarCites <- function(scholar.id, .Encoding="UTF-8", gs=NULL)
{
  limit <- 100  # not more than 100
  ps <- ifelse(limit <= 20, 20, 100)
  .params <- list(hl="en", user=scholar.id, oe=.Encoding, 
                  pagesize=ps, view_op="list_works", cstart=0)
  uri <- "http://scholar.google.com/citations"
  if (is.null(gs)) {
    gs <- GET(uri, query=.params)
  }
  doc <- content(gs, type="text/html", encoding="UTF-8")
  x <- xml_find_all(doc, "//tr[@class=\"gsc_a_tr\"]")
  cites <- xml_text(xml_find_all(x, "//td/a[@class=\"gsc_a_ac gs_ibl\"]"))
  cites <- sapply(as.integer(cites), function(y) ifelse(is.na(y), 0, y))
  titles <- xml_text(xml_find_all(x, "//td/a[@class=\"gsc_a_at\"]"))
  titles <- sapply(titles, function(y) strsplit(gsub("[^[:alnum:] ]", "", tolower(y)), " +")[[1]][[1]])
  years <- xml_text(xml_find_all(x, "//td/span[@class=\"gsc_a_h gsc_a_hc gs_ibl\"]"))
  authors <- xml_text(xml_find_all(x, "//td/div[@class=\"gs_gray\"]"))
  authors <- authors[seq(from=1, by=2, to=length(authors))]
  authors <- sapply(authors, function(a) strsplit(tolower(a), ", ", fixed=T)[[1]][1])
  authors <- sapply(authors, word, -1)
  keys <- apply(cbind(authors, years, titles), 1, paste, collapse="", sep="")
  names(cites) <- keys
  aggstats <- as.integer(xml_text(xml_find_all(doc, "//td[@class=\"gsc_rsb_std\"]")))
  list(pubs=cites,
       cites=aggstats[1],
       hindex=aggstats[3],
       gs=gs)
}