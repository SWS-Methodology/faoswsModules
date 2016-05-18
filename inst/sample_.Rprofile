# This .Rprofile file is best placed in your home directory (`path.expand("~")`)
# If that doesn't work, you may need to set an R_PROFILE_USER variable.
#
# Things you might want to change

# options(papersize="a4")
# options(editor="notepad")
# options(pager="internal")

# set the default help type
# options(help_type="text")
  options(help_type="html")

# set a site library
# .Library.site <- file.path(chartr("\\", "/", R.home()), "site-library")

# set a CRAN mirror
local({#r <- getOption("repos")
       #r["faoswsCRAN"] <- "http://hqlprsws1.hq.un.fao.org/fao-sws-cran/"

      r <- structure(c(
      "http://hqlprsws1.hq.un.fao.org/fao-sws-cran/",
      "https://cran.rstudio.com/",
      "http://www.stats.ox.ac.uk/pub/RWin"
      ),
      .Names = c("faoswsCRAN", "CRAN", "CRANextra"), RStudio = TRUE)

      options(repos=r)

      })

# Give a fortune cookie, but only to interactive sessions
# (This would need the fortunes package to be installed.)
# if (interactive())
#   fortunes::fortune()

