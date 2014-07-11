acpPoster
=========

Poster Scoring Program in R

Input is a text file with the ".txt" extension. Each line represents a single judge's scores.

  Input format
  Judge poster score poster score poster score ...

Judge name can have no spaces.  Tabs or spaces are ok between fields

Adjust the variables as needed:
  f <- names of files for each category
  fnames <- descriptive names for f
  fdir <- directory name
  judge.min <- minimum number of posters a judge must evaluate to allow standardization (at least 3)
  poster.min <- each poster should be judged by at least this number

Debug var can be adjusted as needed

At the present time, this will work best for a single category of posters and use separate runs for different categories if present.
