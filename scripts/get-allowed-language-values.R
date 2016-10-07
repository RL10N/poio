# Make a list of all known languages.
# This is currently
# 1. ISO 639-1 two letter language codes + optional ISO 3166 alpha-2 country codes
# 2. ISO 639-2 three letter language codes
#
# The easiest thing to do would be to grab these datasets from the ISOcodes
# package.  However, it isn't clear whether GNU gettext supports all these
# languages.  From the documentation, it looks like at leastISO 639-2 isn't
# completely supported.
# Consequently we scrape the online documentation instead.

library(httr)
library(rvest)

urls <- c(
  usual_lang = "https://www.gnu.org/software/gettext/manual/html_node/Usual-Language-Codes.html#Usual-Language-Codes",
  rare_lang = "https://www.gnu.org/software/gettext/manual/html_node/Rare-Language-Codes.html#Rare-Language-Codes",
  country = "https://www.gnu.org/software/gettext/manual/html_node/Country-Codes.html#Country-Codes"
)

language_codes <- lapply(
  urls,
  function(u)
  {
    response <- GET(u)
    stop_for_status(response)
    page <- content(response)

    page %>%
      xml_nodes(xpath = "//samp") %>%
      xml_text()
  }
)

save(language_codes, file = "data/language_codes.RData")
