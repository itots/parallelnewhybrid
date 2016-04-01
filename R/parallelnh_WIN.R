#' @name parallelnh_WIN
#' @title Parallel NewHybrids - Windows version
#'
#' @description \code{paralllelnh_WIN} is the Windows version of the function that allows NewHybrids (Anderson XXXX) to be run in parallel. It does so by creating a vector of file names to be run, that are held within a single folder. paralleleNH_WIN then assigns a job to each of the cores available in the computer. As each task finishes, parallelnh_WIN assigns a new analysis to the idle core.
#' @param folder.data A file path to the folder in which the NewHybrids formatted files to be analyzed reside
#' @param where.NH A file path to the NewHybrids installation folder
#' @param burnin An integer specifying how many burnin steps NewHybrids is to run
#' @param sweeps An integer specifying how many sweep steps NewHybrids is to run
#' @export
#' @import parallel
#' @import plyr
#' @import stringr


parallelnh_WIN <- function(folder.data, where.NH, burnin, sweeps){

  useroptions <- options()
  options(scipen = 999) ## have to change the global R settings because NH doesn't understand scientific notation' - will change back on exit

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

  NH.loc.vec <- paste(where.temp, "newhybrids", sep = "/") ## put some folders up in that temp dir

  ## get some copying going
  for(i in 1:length(files.anal)){
    file.copy(from = where.NH, to = where.temp, recursive = TRUE) ## copies whole folder -- not as necessery in Windows version
    NH.rename <- paste(NH.loc.vec, i, sep="_") ## get vector of where the folder is, then add a number to it
    file.rename(from = NH.loc.vec, to = NH.rename) ## rename
      }

  ## Copy the files to be analzyed into the NH folder copies - one file per copy
  NH.copies <- list.files(where.temp) ## list of copies of NH in the temp folder

  ## slide dem files.anal in
  for(j in 1:length(files.anal)){ ## for each file to be analyzed
    to.file <- NH.copies[j] ## what copy of NH do you copy the file to be analyzed to?
    from.file <- files.anal[j] ## the file to be copied
    to.dir <- paste0(where.temp, to.file, "/") ## where is the NH copy - this is a directory
    from.dir <- paste0(folder.data, from.file) ### where is the data file to be copied
    file.copy(from = from.dir, to = to.dir) ## copy it on ovah
      }


  NH.copy.list <- list.files(path = where.temp)

  ## I think you can see what these do
  burnin.do <- paste("--burn-in", burnin, sep=" ")
  sweeps.do <- paste("--num-sweeps", sweeps, sep=" ")

  ## add in randomized seeds - the NH guide says they should be small.
  r.seed <- sample(x = c(1:10), size = 2)

    do.seed <- c("--seeds", r.seed)

  ## you know I love them NULL vecssssss
  jobs.vector <- NULL

  ### commmands: what files and how to anal them?

  for(b in 1:length(NH.copy.list)){
    b.copy <- NH.copy.list[b]
    file.do <- paste("-d", files.anal[b])
    what.temp <- paste0(where.temp, b.copy)
    path.hold <- paste("cd", paste0(what.temp, " &"), "newhybrids.exe", file.do, burnin.do, sweeps.do, "--no-gui", sep = " ") ## must separate
    ## the get directory form teh command with & in Windows version
    jobs.vector <- rbind(jobs.vector, path.hold) ### rbind - will use ROW apply later
      }

  ## make slaves
  mc.cores <- min(length(jobs.vector), detectCores())
  makecl <- makeCluster( mc.cores, outfile="" )

  ### parallele row apply - apply each command in the vector to the shell =
  parRapply(cl = makecl, x=jobs.vector, FUN=shell)



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

  ## gotta stop the slaves somehow
  #stopCluster(makecl) ### I think this works???

  library(snow)
  stopCluster(makecl)
  mpi.quit()

  unlink(paste0(folder.data, "NH.Temp", "/"), recursive = TRUE)

  on.exit(options(useroptions))


}
