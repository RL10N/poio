[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active)
[![Travis-CI Build Status](https://travis-ci.org/RL10N/poio.svg?branch=master)](https://travis-ci.org/RL10N/poio)

# **poio**: Input/Output Functionality for "PO" and "POT" Message Translation Files

R packages use a text file format with a `.po` extension to store translations of messages, warnings, and errors.  **poio** provides functionality to read in these files, fix the metadata, and write the objects back to file.

## Installation

To install the development version, you first need the *devtools* package.


```r
install.packages("devtools")
```

Then you can install the **poio** package using


```r
devtools::install_bitbucket("RL10N/poio")
```

## Functions

`read_po` reads PO and POT files into R, and stores them as an object of class `"po"` (see below for details).

`fix_metadata` fixes the metadata in a `po` object.

`generate_po_from_pot` generates a PO object from a POT object.

`write_po` writes `po` objects back to a PO file.

`get_n_plural_forms` is a convenience function for retriving the number of plural forms for a language from the `po` object's metadata.

## Datasets

`language_codes` is a list of all the language codes and the country codes that `gettext` understands.

`plural_forms` is a data.frame of the plural forms header string for over 140 common languages. 

## Examples

A typical workflow begins by generating a POT master translation file for a package using `tools::xgettext2pot`.  In this case, we'll use a sample file stored in the **poio** package.  The contents look like this:


```r
pot_file <- system.file("extdata/R-summerof69.pot", package = "poio")
cat(readLines(pot_file), sep = "\n")
```

```
## # This is a translator comment before the metadata.
## # Other comment types aren't useful here, and should be ignored.
## # Like the "fuzzy" flags comment below.
## #, fuzzy
## msgid ""
## msgstr ""
## "Project-Id-Version: R 3.3.1\n"
## "Report-Msgid-Bugs-To: bugs.r-project.org\n"
## "POT-Creation-Date: 2016-10-05 20:19\n"
## "PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\n"
## "Last-Translator: FULL NAME <EMAIL@ADDRESS>\n"
## "Language-Team: LANGUAGE <LL@li.org>\n"
## "MIME-Version: 1.0\n"
## "Content-Type: text/plain; charset=CHARSET\n"
## "Content-Transfer-Encoding: 8bit\n"
## 
## # Now commencing Bryan Adams lyrics
## # Because the song has numbers in it
## #: some_source_file.R:123
## msgid "I got my first real six-string"
## msgstr ""
## 
## 
## 
## 
## #        This one gets a "c-format" flags comment
## #because it uses c-style sprintf formatting
## #, c-format
## msgid "Bought it at the %f-and-dime"
## msgstr ""
## 
## # I don't think that the tools package supports generating
## # source reference comments, but we should preserve them
## # in case someone manually inserts them into their file
## #: some_source_file.R:123
## msgid "Played it till my fingers bled"
## msgstr ""
## 
## # Also uses xgettextf
## #, c-format
## msgctxt "Summer as in seasons, not a function that calculates sums"
## msgid "It was the summer of '%d."
## msgstr ""
## 
## # Technically the lyric says 'some' guys
## # but I want a countable example
## msgid        "Me and %d guy from school"
## msgid_plural "Me and %d guys from school"
## msgstr[0]    ""
## msgstr[1]    ""
## 
## # Testing quote escaping
## msgid "Had a \"band\"" and we tried real hard"
## msgstr ""
## 
## # Obsolete messages can also have other comments
## #~ msgid "Jimmy quit and Jody got married"
## #~ msgstr ""
## 
## # Countably obsolete. Apologies for bad English.
## # Also note the bad number order.
## #~ msgid "I should've known we'd never get far"
## #~ msgid_plural "I should've known we'd never get fars"
## #~ msgstr[1]""
## #~ msgstr[0]         ""
```

To import the file, use `read_po`.  A description on the object's structure is shown in the "PO Objects" section below.


```r
(pot <- read_po(pot_file))
```

```
## $source_type
## [1] "r"
## 
## $file_type
## [1] "pot"
## 
## $initial_comments
## [1] "This is a translator comment before the metadata."             
## [2] "Other comment types aren't useful here, and should be ignored."
## [3] "Like the \"fuzzy\" flags comment below."                       
## 
## $metadata
## # A tibble: 9 × 2
##                        name                       value
##                       <chr>                       <chr>
## 1        Project-Id-Version                     R 3.3.1
## 2      Report-Msgid-Bugs-To          bugs.r-project.org
## 3         POT-Creation-Date            2016-10-05 20:19
## 4          PO-Revision-Date       YEAR-MO-DA HO:MI+ZONE
## 5           Last-Translator   FULL NAME <EMAIL@ADDRESS>
## 6             Language-Team        LANGUAGE <LL@li.org>
## 7              MIME-Version                         1.0
## 8              Content-Type text/plain; charset=CHARSET
## 9 Content-Transfer-Encoding                        8bit
## 
## $direct
## # A tibble: 6 × 8
##                                      msgid msgstr is_obsolete   msgctxt
##                                      <chr>  <chr>       <lgl>    <list>
## 1           I got my first real six-string              FALSE <chr [0]>
## 2             Bought it at the %f-and-dime              FALSE <chr [0]>
## 3           Played it till my fingers bled              FALSE <chr [0]>
## 4                It was the summer of '%d.              FALSE <chr [1]>
## 5 Had a \\"band\\"" and we tried real hard              FALSE <chr [0]>
## 6          Jimmy quit and Jody got married               TRUE <chr [0]>
## # ... with 4 more variables: translator_comments <list>,
## #   source_reference_comments <list>, flags_comments <list>,
## #   previous_string_comments <list>
## 
## $countable
## # A tibble: 2 × 9
##                                  msgid
##                                  <chr>
## 1            Me and %d guy from school
## 2 I should've known we'd never get far
## # ... with 8 more variables: msgid_plural <chr>, msgstr <list>,
## #   is_obsolete <lgl>, msgctxt <list>, translator_comments <list>,
## #   source_reference_comments <list>, flags_comments <list>,
## #   previous_string_comments <list>
## 
## attr(,"class")
## [1] "po"   "list"
```

`tools::xgettext2pot` makes a mess of some of the metadata element that it generates, so they need fixing.


```r
pot_fixed <- fix_metadata(pot)
```

```
## Updating the Project-Id-Version to 'poio 0.0-1'.
```

```
## Updating the Report-Msgid-Bugs-To to 'https://github.com/RL10N/poio/issues'.
```

```
## Updating the PO-Revision-Date to '2016-10-30 21:12:41+0300'.
```

```
## Updating the Content-Type to 'text/plain; charset=UTF-8'.
```

Now you need to choose some languages to translate your messages into.  Suitable language codes can be found in the `language_codes` dataset included in the package.


```r
data(language_codes)
str(language_codes, vec.len = 8)
```

```
## List of 2
##  $ language: chr [1:245] "aa" "ab" "ace" "ae" "af" "ak" "am" "an" ...
##  $ country : chr [1:249] "AD" "AE" "AF" "AG" "AI" "AL" "AM" "AO" ...
```

Then, for each language that you want to create a translation for, generate a `po` object and write it to file. If your current working directory is the root of your package, the correct file name is automatically generated.


```r
for(lang in c("de", "fr_BE"))
{
  po <- generate_po_from_pot(pot, lang)
  write_po(po)
}
```

## PO Objects

`po` objects are lists with class `"po"` (to allow S3 methods), containing the following elements:

- *source_type*: A string.  Either `"r"` or `"c"`, depending upon whether the messages originated from R-level code, or C-level code.
- *file_type*: Either `"po"` or `"pot"`, depending upon whether the messages originated from a PO (language-specific) or POT (master translation) file. Determined from the file name.
- *initial_comments*: A `character` vector of comments added by the translator.
- *metadata*: A `data_frame` of file metadata with columns "name" and "value".
- *direct*: A `data_frame` of messages with a direct translation, as created by `stop`,
`warning`, `message` or`gettext`; its columns are described below.
- *countable*: A `data_frame`of messages where the translation depends upon a countable value, as created by `ngettext`; its columns are described below.

The `direct` element of the `po` object has the following columns.

- *msgid*: Character. The untranslated (should be American English) message.
- *msgstr*: Character. The translated message, or empty strings in the case of POT files.
- *is_obsolete*: Logical. Is the message obsolete?
- *msgctxt*: List of character. Disambiguating context information to allow multiple messages with the same ID.
- *translator_comments*: List of character. Comments added by the translator, typically to explain unclear messages, or why translation choices were made.
- *source_reference_comments*: List of character. Links to where the message occured in the source, in the form "filename:line".
- *flags_comments*: List of character. Typically used to describe formatting directives. R uses C-style formatting, which would imply a `"c-format"` flag.  For example `%d` denotes an integer, and `%s` denotes a string. `"fuzzy"` flags can appear when PO files are merged.
- *previous_string_comment*: List of character. When PO files are merged with an updated POT file ,and a fuzzy flag is generated, the old msgid is stored in a previous string comment.

The `countable` element of the `po` object takes the same form as the `direct` element, with two differences.

- *msgid_plural*: Character. The plural form of the untranslated message.
- *msgstr*: This is now a list of character (rather than character.)

## See Also

The [*msgtools*](http://github.com/RL10N/msgtools) package, which has higher level tools for working with messages and translations.

The Pology python library has some useful [*documentation on the PO file format*](http://pology.nedohodnik.net/doc/user/en_US/ch-poformat.html).

The GNU [*gettext*](https://www.gnu.org/software/gettext/manual/html_node/index.html) utility.

## Acknowledgements

This package was developed as part of the [*RL10N*](https://rl10n.github.io) project, funded by the [R Consortium](https://www.r-consortium.org/).

