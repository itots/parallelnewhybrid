#' @name parallelnh_LINUX
#' @title Parallel NewHybrids - LINUX version
#'
#' @description \code{paralllelnh_OSX} is the LINUX (Ubuntu) version of the function thatallows NewHybrids (Anderson XXXX) to be run in parallel. It does so by creating a vector of file names to be run, that are held within a single folder. paralleleNH_LINUX then assigns a job to each of the cores available in the computer. As each task finishes, parallelnh_LINUX assigns a new analysis to the idle core.
#' @param folder.data A file path to the folder in which the NewHybrids formatted files to be analyzed reside
#' @param where.NH A file path to the NewHybrids installation folder
#' @param burnin An integer specifying how many burnin steps NewHybrids is to run
#' @param sweeps An integer specifying how many sweep steps NewHybrids is to run
#' @export
#' @import parallel
#' @import plyr
#' @import stringr


parallelnh_LINUX <- function(folder.data, where.NH, burnin, sweeps){

  useroptions <- options()

  options(scipen = 999) ### if the number of digits is too big in either burnin or sweeps
    ## R will output in scientific notation, which is not interpreted correctly by NHm - will change back on exit

  files.anal <- list.files(path = folder.data)

  path.indiv.file <- NULL
  indiv.file.exists <- grep(pattern = "individuals.txt", x = files.anal)

    if(indiv.file.exists > 0){
      path.indiv.file <- paste0(folder.data, files.anal[indiv.file.exists])
      files.anal <- files.anal[-indiv.file.exists]
    }
    if(indiv.file.exists == 0){
      writeLines("Note: An individual file has not been provided. Please refer to the help file for more information")
    }

    dir.create(path = paste0(folder.data, "NH.Temp"))
    where.temp <- paste0(folder.data, "NH.Temp", "/")


    ## create a new folder to put the results in - remember the inception thing - be smart
    dir.create(path = paste0(folder.data, "NH.Results"))
    res.path.make <- paste0(folder.data, "NH.Results") ## get the path to the new results folder

    h.cores <- detectCores()



    ## Copy the entire NH folder i times to the temp folder you made
    ## use a loop here because then can iteratively name the copies so that the file.copy doesn't
    ## lose it because you have things named the same - also - how else are you goign to ID things later?

    NH.loc.vec <- paste(where.temp, "newhybrids", sep = "/") ##
    for(i in 1:length(files.anal)){

        file.copy(from = where.NH, to = where.temp, recursive = TRUE) ## copies whole folder

        NH.rename <- paste(NH.loc.vec, i, sep="_") ## get vector of where the folder is, then add a number to it

        file.rename(from = NH.loc.vec, to = NH.rename) ## rename


    }


    ## Copy the files to be analzyed into the NH folder copies - one file per copy
    NH.copies <- list.files(where.temp) ## list of copies of NH in the temp folder

    for(j in 1:length(files.anal)){ ## for each file to be analyzed


        to.file <- NH.copies[j] ## what copy of NH do you copy the file to be analyzed to?
        from.file <- files.anal[j] ## the file to be copied

        to.dir <- paste0(where.temp, to.file, "/") ## where is the NH copy - this is a directory
        from.dir <- paste0(folder.data, from.file) ### where is the data file to be copied

        file.copy(from = from.dir, to = to.dir) ## copy it on ovah

    }

    ## once NH has run, need to extract (copy) the output

    ### get a list of all the copies of NH you have made etc.
    NH.copy.list <- list.files(path = where.temp)
    # NH.copy.list # for internal code checking


    ## now to execute the command

    ## add in randomized seeds - the NH guide says they should be small.

   r.seed <- sample(x = c(1:10), size = 2)

    do.seed <- c("--seeds", r.seed)
    burnin.do <- paste("--burn-in", burnin, sep=" ")
    sweeps.do <- paste("--num-sweeps", sweeps, sep=" ")
    ## replace with files.anal
    where.temp2 <- gsub(x = where.temp, pattern = " ", replacement = "\\ ", fixed = T)
    jobs.vector <- NULL

    for(b in 1:length(NH.copy.list)){

        b.copy <- NH.copy.list[b]
        file.do <- paste("-d", files.anal[b])
        what.temp <- paste0(where.temp2, b.copy)
        path.hold <- paste("cd", paste0(what.temp, ";"), "./newhybrids-no-gui-linux.exe", file.do, burnin.do, sweeps.do, "--no-gui", sep = " ")
        jobs.vector <- c(jobs.vector, path.hold)

    }


    if(length(jobs.vector)<h.cores){
        mclapply(X = jobs.vector, FUN = system, mc.cores = length(jobs.vector))
    }
    if(length(jobs.vector)>h.cores){
        mclapply(X = jobs.vector, FUN = system, mc.cores = h.cores)
    }



## get the results generated for each copy of NH/file to be analyzed
  for(k in 1:length(NH.copy.list)){

    ## make a nice name for the folders of resutls we are goign to make - so so pretty
    res.name <- paste0(files.anal[k], "_Results") # file name + results - not as pretty as it could be
    dir.create(path = paste0(res.path.make, "/", res.name)) ## make that folder to put the resutls in

    where.the.results.to <- paste0(res.path.make, "/", res.name, "/") ## where did you make that folder?

    ## copy in the proper data file from which the sims were made
    file.copy(from = paste0(folder.data, files.anal[k]), to = where.the.results.to)
    ## copy in the individual file
    file.copy(from = paste0(path.indiv.file), to = where.the.results.to)

    NH.copy.to.get <- NH.copy.list[k] ## what copy of NH are we goign to look in?
    find.res.vec <- list.files(path = paste0(where.temp, NH.copy.to.get), pattern = "aa-") ## find all the files that
    ## begin with "aa-" inside that copy of NH - big shout out to Eric Anderson for using a regular string in the results name!

    find.res.vec.path <- paste0(paste0(where.temp, NH.copy.to.get, "/"), find.res.vec) ## find the path to all the files ID'd
    file.copy(from = find.res.vec.path, to = where.the.results.to) ## copy those files to the proper results folder

    ## keep making it pretty by renaming the results files so you know what data set they are associated with
    rename.vec <- gsub(x = find.res.vec, pattern = "aa-", replacement = paste0(files.anal[k], "_"))

    ## again - need a vectr that shows their computr postion
    rename.vec.path <- paste0(where.the.results.to, rename.vec) ## vector with new names
    old.name.vec.path <- paste0(where.the.results.to, find.res.vec) ## vector with old names

    file.rename(from = old.name.vec.path, to = rename.vec.path) ## check that vectorized functionality in file.rename - mad props!
      }

    unlink(paste0(folder.data, "NH.Temp", "/"), recursive = TRUE)

    on.exit(options(useroptions))

}
