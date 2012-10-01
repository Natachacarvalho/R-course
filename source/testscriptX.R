> complete.testscript
function() {
   checkSanity()
   outputfiles <- sprintf("complete-testoutput%d.txt", 1:3)

   op <- options(); options(scipen = 4); on.exit(options(op))

   source("getmonitor.R", local = TRUE)
   source("complete.R", local = TRUE)

   ## Run all monitors
   cc <- complete("specdata", 332:1)
   stopifnot(all(names(cc) %in% c("id", "nobs")))
   cc$id <- as.integer(cc$id)
   cc <- cc[order(cc$id), ]
   set.seed(248)
   out <- as.numeric(head(cc[sample(332, 10), "nobs"]))
   writeLines(as.character(out), outputfiles[1])

   ## Single monitor
   cc <- complete("specdata", 24)
   writeLines(as.character(as.numeric(cc$nobs)), outputfiles[2])

   ## Subset
   set.seed(42)
   cc <- complete("specdata", sample(332, 10))
   cc$id <- as.integer(cc$id)
   cc <- cc[order(cc$id), ]
   out <- c(as.numeric(cc$id), as.numeric(cc$nobs))
   writeLines(as.character(out), outputfiles[3])

   cat("please upload the files\n\t", paste(outputfiles, collapse = "\n\t"), "\nto the Coursera web site\n", sep = "")
    return(invisible())
  }