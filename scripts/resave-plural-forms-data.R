# Resave plural forms CSV file as an RData object.
# The dataset originated at
# https://localization-guide.readthedocs.io/en/latest/l10n/pluralforms.html
# And was converted to CSV by Thomas Leeper.
# Then migrated from the msgtools package by Richie Cotton.

plural_forms <- read.csv("inst/scripts/PluralFormHeaders.csv", stringsAsFactors = FALSE)

# There are footnote links from the original webpage that need removing.
plural_forms$EnglishName[plural_forms$EnglishName == "Arabic [1]"] <- "Arabic"
plural_forms$EnglishName[plural_forms$EnglishName == "Chinese [2]"] <- "Chinese"

# For the purposes of auto-replacing the plural form, we can deal with the
# special case of Mandarin Chinese in code rather than keeping it in the dataset
plural_forms <- plural_forms[plural_forms$EnglishName != "Chinese [3]", ]

save(plural_forms, file = "data/plural_forms.RData")
