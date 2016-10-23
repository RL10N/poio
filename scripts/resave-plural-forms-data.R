# Resave plural forms CSV file as an RData object.
# The dataset originated at
# https://localization-guide.readthedocs.io/en/latest/l10n/pluralforms.html
# And was converted to CSV by Thomas Leeper.
# Then migrated from the msgtools package by Richie Cotton.

plural_forms <- data.table::fread(
  "scripts/PluralFormHeaders.csv",
  stringsAsFactors = FALSE,
  encoding = "UTF-8",
  data.table = FALSE
)

# There are footnote links from the original webpage that need removing.
plural_forms$EnglishName[plural_forms$EnglishName == "Arabic [1]"] <- "Arabic"
plural_forms$EnglishName[plural_forms$EnglishName == "Chinese [2]"] <- "Chinese"

# For the purposes of auto-replacing the plural form, we can deal with the
# special case of Mandarin Chinese in code rather than keeping it in the dataset
plural_forms <- plural_forms[plural_forms$EnglishName != "Chinese [3]", ]

# Macedonian has some additional text in the plural forms description
plural_forms$PluralFormHeader[plural_forms$EnglishName == "Macedonian"] <- "nplurals=2; plural=(n==1 || n%10==1 ? 0 : 1);"

# Aymara shouldn't have an acute on the last "a"
plural_forms$EnglishName[stringi::stri_detect_fixed(plural_forms$EnglishName, "Aymar")] <- "Aymara"


save(plural_forms, file = "data/plural_forms.RData")
