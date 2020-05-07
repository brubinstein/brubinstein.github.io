# script : rmd-bib-util.R
# author : Benjamin Rubinstein
# about  : extracts, processes and prints custom rmdnote field in a
#          biblatex file (using R package RefManageR). This field should
#          have the format kvp1;kvp2;...;kvpN where each has format
#          key=value. The current supported keys can be found as the names
#          of the rmdfs list

library("RefManageR")

# rmd note functions, for appending special web-only notes to bib entries
rmdfs <- list()
rmdfs$pdf <- function(val) paste("[PDF](", val, ")", sep="")
rmdfs$full <- function(val) paste("[full](", val, ")", sep="")
rmdfs$code <- function(val) paste("[software](", val, ")", sep="")
rmdfs$data <- function(val) paste("[dataset](", val, ")", sep="")
rmdfs$pop <- function(val) paste("[pop article](", val, ")", sep="")
rmdfs$pub <- function(val) paste("[publisher link](", val, ")", sep="")
rmdfs$fix <- function(val) paste("[corrigendum](", val, ")", sep="")
rmdfs$alert <- function(val) paste('<span style="color:red">', val, "</span>", sep="")

# Prints a custom "rmdnote" list key-value-pairs included with a bib entry
# - bibent    A bibentry extracted from RefManageR
# - rmdFuncs  A list of functions for processing possible rmdnote keys
# - append    Any text to include at the end of the entry post-note
# - bold      Logical, whether to make bold, the note in printing
# Prints the note if there is one, or if there is any append note;
# otherwise prints nothing.
print.rmdnote <- function(bibent,
                          rmdFuncs = rmdfs, 
                          append = NULL, 
                          bold = TRUE) {
  if (!is.null(append) || !is.null(bibent$rmdnote)) {
    # there is either a rmdnote field, or an append note
    cat("[")
    anyRmd <- FALSE
    if (!is.null(bibent$rmdnote)) {
      # there is a rmdnote field
      kvps <- unlist(strsplit(bibent$rmdnote, ";", fixed = TRUE))
      for (kvp in kvps) {
        kvp <- unlist(strsplit(kvp, "=", fixed = TRUE))
        if (anyRmd) {
          cat("; ")
        } else {
          anyRmd <- TRUE
        }
        if (length(kvp) >= 2) {
          if (length(kvp) > 2) {
            kvptemp <- c(kvp[1], paste(tail(kvp,-1), sep="=", collapse="="))
            kvp <- kvptemp
          }
          f <- rmdFuncs[[kvp[1]]]
          v <- kvp[2]
          if (!is.null(f)) {
            # found a rmdnote processing function
            if (bold) cat("**")
            cat(f(v))
            if (bold) cat("**")
          } else {
            # expected but couldn't find a processing function
            cat("**ERR2**")
          }
        } else {
          # expected both key and value, but didn't find both
          cat("**ERR1**")
        }
      }
    }
    if (!is.null(append)) {
      # there is an append note
      if (anyRmd) {
        cat("; ")
      }
      if (bold) cat("**")
      cat(append)
      if (bold) cat("**")
    }
    cat("]\n")
  }
}

# Print a hidden bibtex source for a given biblatex entry with
# HTML tag ID being "bibtex-key" where key is the bibentry key.
# Hide it such that no space vertical/horizontal is used at all.
bibtex.src <- function(bibent) {
  cat("<!--html_preserve-->\n")
  cat('<div class="bibox" style="display: none;">\n')
  cat('<pre><code id="bibtex-', bibent$key, '">\n', sep="")
  print(toBibtex(bibent))
  cat('</code></pre></div>\n')
  cat("<!--/html_preserve-->\n\n")
}

# Print an HTML link that calls javascript for extracting a hidden
# bibtex source from the page by ID being this bibent's key prepended
# with "bibtex-", encodes the bibtex source as a file, and begins
# download. The cursor is made (hopefully) the same as normal links.
bibtex.lnk <- function(bibent, lnk.txt = "BibTeX") {
  s <- bibent$key
  s1 <- "<!--html_preserve--><a"
  s2 <- ' style="cursor: pointer;" onclick="DownloadToFile('
  s3 <- paste("'", s, ".bib', 'bibtex-", s, "'", sep="")
  s4 <- paste(')">', lnk.txt, '</a><!--/html_preserve-->', sep="")
  paste(s1, s2, s3, s4, sep="")
}
