# Make a list of all known languages.
# This is currently
# 1. ISO 639-1 two letter language codes, or
# 2. Some ISO 639-2 three letter language codes
# In either case, an optional ISO 3166 alpha-2 country code suffix is allowed.
#
# The easiest thing to do would be to grab these datasets from the ISOcodes
# package.  This is possible for country codes.
# However, GNU gettext doesn't supports all these languages.  For
# example, ISO 639-2 is only supported for rare languages where there is no ISO
# 629-1 code.
# Consequently we scrape the online documentation instead.

library(ISOcodes)
library(httr)
library(rvest)
library(dplyr)

data("ISO_639_2")
data("ISO_3166_1")

# Alternate ISO data sources (but ISOcodes is more convenient)
# iso639_1 <- read.delim(
#   "http://id.loc.gov/vocabulary/iso639-1.tsv",
#   stringsAsFactors = FALSE,
#   na.strings = ""
# )
# iso639_2 <- read.delim(
#   "http://id.loc.gov/vocabulary/iso639-2.tsv",
#   stringsAsFactors = FALSE,
#   na.strings = ""
# )
# iso3166_alpha2 <- read.csv(
#   "https://raw.githubusercontent.com/datasets/country-list/master/data.csv",
#   stringsAsFactors = FALSE,
#   na.strings = ""
# )

urls <- c(
  usual_lang = "https://www.gnu.org/software/gettext/manual/html_node/Usual-Language-Codes.html#Usual-Language-Codes",
  rare_lang = "https://www.gnu.org/software/gettext/manual/html_node/Rare-Language-Codes.html#Rare-Language-Codes"
)
# Country codes are likewise available from
# "https://www.gnu.org/software/gettext/manual/html_node/Country-Codes.html#Country-Codes"

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

# Merge 2-letter lang codes
iso639_1 <- ISO_639_2 %>%
  filter_(~ !is.na(Alpha_2)) %>%
  select_(~ Alpha_2, ~ Name)

usual_lang <- data.frame(
  Alpha_2 = language_codes$usual_lang,
  stringsAsFactors = FALSE
) %>%
  left_join(iso639_1)

# mo = Moldovian is not included in ISO but is in gettext
usual_lang$Name[usual_lang$Alpha_2 == "mo"] <- "Moldavian; Moldovan"

language_codes$usual_lang <- with(usual_lang, setNames(Alpha_2, Name))

# Merge 3-letter lang codes
# There are two variants of 3-letter codes. See ?ISOcodes::ISO_639
# B = bibliographic
# T = terminology
# In this case all(language_codes$rare_lang %in% ISO_639_2$Alpha_3_T)
# and all(language_codes$rare_lang %in% ISO_639_2$Alpha_3_B)
# so it doesn't matter
iso639_2 <- ISO_639_2 %>%
  select_(~ Alpha_3_B, ~ Name)

rare_lang <- data.frame(
  Alpha_3_B = language_codes$rare_lang,
  stringsAsFactors = FALSE
) %>%
  left_join(iso639_2)

language_codes$rare_lang <- with(rare_lang, setNames(Alpha_3_B, Name))


# Just take country codes directly from ISO dataset
language_codes$country <- with(ISO_3166_1, setNames(Alpha_2, Name))

# For convenience, merge usual and rare languages
language_codes <- with(
  language_codes,
  list(
    language = sort(c(usual_lang, rare_lang)),
    country = country
  )
)

save(language_codes, file = "data/language_codes.RData")
