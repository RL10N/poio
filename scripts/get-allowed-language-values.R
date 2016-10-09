# Make a list of all known languages.
# This is currently
# 1. ISO 639-1 two letter language codes, or
# 2. Some ISO 639-2 three letter language codes
# In either case, an optional ISO 3166 alpha-2 country code suffix is allowed.
#
# The easiest thing to do would be to grab these datasets from the ISOcodes
# package.  However, GNU gettext doesn't supports all these languages.  For
# example, ISO 639-2 is only supported for rare languages where there is no ISO
# 629-1 code.
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

# For convenience, merge usual and rare languages
language_codes <- with(
  language_codes,
  list(
    language = sort(c(usual_lang, rare_lang)),
    country = country
  )
)

save(language_codes, file = "data/language_codes.RData")
