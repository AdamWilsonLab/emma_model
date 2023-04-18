#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.


# authorize github.  Note this assumes there is an environmental variable
  # called 'GITHUB_PAT", which contains your PAT

  gitcreds::gitcreds_approve(list(protocol = "https",
                                  host = "github.com",
                                  username = NULL,
                                  password = Sys.getenv("GITHUB_PAT")))

#Run targets

  targets::tar_make()
# targets::tar_make_clustermq(workers = 2) # nolint
# targets::tar_make_future(workers = 2) # nolint
