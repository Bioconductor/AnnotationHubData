### =========================================================================
### Utility functions
### -------------------------------------------------------------------------
###

constructSeqInfo <- function(species, genome)
{
  recognized.human <- species=="Homo sapiens" & genome %in% c("hg18", "hg19")
  recognized.mouse <- species=="Mus musculus" & genome %in% c("mm10")
  recognized <- recognized.human | recognized.mouse
  stopifnot(recognized)
 
  suppressMessages({
       # chroms 1-22, X, Y, M are assumed to be the first 25 rows of the
       # data.frame
     if(recognized.human)
        tbl.chromInfo =
            GenomicFeatures:::.makeUCSCChrominfo (genome,
                                                  circ_seqs="chrM") [1:25,]
     if(recognized.mouse)
        tbl.chromInfo =
            GenomicFeatures:::.makeUCSCChrominfo (genome,
                                                  circ_seqs="chrM") [1:22,]
 
     })

   Seqinfo(as.character(tbl.chromInfo$chrom), 
           seqlengths=tbl.chromInfo$length, 
           isCircular=tbl.chromInfo$is_circular,
           genome=genome)
}

.sortTableByChromosomalLocation <- function(tbl)
{
  stopifnot (all (c ('seqname', 'start') %in% colnames (tbl)))
  factor.chromNames <- factor (tbl$seqname,
                               levels=paste("chr", c(1:22, "X", "Y", "M"),
                                            sep=''))
  tbl$seqname <- factor.chromNames
  tbl <- tbl [order (tbl$seqname, tbl$start), ]
  invisible (tbl)

} 

.printf <- function(...) print(noquote(sprintf(...)))

## from ?grep, by Luke Tierney
URL_parts <- function(x)
{
    m <- regexec("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)", x)
    parts <- do.call(rbind,
       lapply(regmatches(x, m), `[`, c(3L, 4L, 6L, 7L)))
    colnames(parts) <- c("protocol","host","port","path")
    parts
}

## log messages to console AND to a file
flog <- function(level, ...)
{
    loggerFunction <- switch(level,
        "flog.fatal",
        "flog.error",
        "noop",
        "flog.warn",
        "noop",
        "flog.info",
        "noop",
        "flog.debug",
        "flog.trace"
        )
    dots <- list(...)
    do.call(loggerFunction, dots)
    dots$name <- "file"
    do.call(loggerFunction, dots)
}

## Uploading to S3 usually happens in AnnotationHubServer, but 
## when running the track*Recipe recipes, it happens in
## AnnotationHubData. There is an RAmazonS3 R package but 
## it does not work well for uploading files. Therefore this
## function expects the AWS CLI to be installed. 
## See: https://aws.amazon.com/cli/
## It should be configured with a user who can write to 
## the appropriate bucket. 
upload_to_S3 <- 
    function(file, remotename, bucket, profile, acl="public-read")
{
    remotename <- sub("^\\/", "", remotename)
    #aws --profile ahs_content_uploader s3 cp --acl public-read test s3://annotationhub/loquat/vato/manichean/test
    profileStr <- " "
    if (!missing(profile))
        profileStr <- paste("--profile ", profile)

    cmd <- "aws"
    if (length(file) != length(remotename))
        stop("Length of file does not match length of remotename!")

    for (i in 1:length(file)) {
        thisFile <- file[i]
        thisRemoteName <- remotename[i]
        quotes = getOption("useFancyQuotes")
        on.exit(options(useFancyQuotes=quotes))
        options(useFancyQuotes=FALSE)
        args <- sprintf("%s s3 cp --region us-east-1 --acl %s %s s3://%s/%s",
            profileStr, acl, dQuote(thisFile), bucket, dQuote(thisRemoteName))
        res <- system2(cmd, args)
        if (res != 0)
            stop(sprintf("Failed to upload %s to S3! Result was %s.", file, res))
    }
    
    TRUE
}


## new function to upload to azure

upload_to_azure <-
    function(file, sas)
{
    if(missing(sas)){
        sas = Sys.getenv("AZURE_SAS_URL", NA_character_)
    }
    if(is.na(sas)){
        stop("AZURE_SAS_URL environment variables is not set or given")
    }
    stopifnot(startsWith(prefix="https", sas))
    if(Sys.which("azcopy") == ""){
        stop("Please download azcopy")
    }

    args <- paste0("copy --recursive ", file, " '", sas, "'")
    res <- system2("azcopy", args)
    if (res != 0) stop(sprintf("Failed to upload %s to Azure!", file))
    TRUE
}



globalVariables(c("futile.logger"))

.onLoad <-
    function(libname, pkgname)
{
   logDir <- file.path(Sys.getenv("HOME"),
        sprintf(".%s", pkgname))
    if (!file.exists(logDir))
    {
        .printf("Creating log directory %s", logDir)
        dir.create(logDir)
    }
    l <- library
    l(futile.logger)
    flog.threshold(TRACE)
    flog.appender(appender.file(file.path(logDir,
        sprintf("%s.log", pkgname))), name="file")
}


`%_%` <- function(a, b) paste0(a, b)


# Create "pointer" variables for large data sets.
ptr <- pointer <- function(..., pos=-1, envir=as.environment(pos),
    namedList=TRUE, expandCharacter=FALSE)
{
    variableList <- tail(as.list(match.call()), -1)

    if (length(variableList) == 0)
        stop("Must supply reference object.")

    exclusions <- intersect(names(variableList), setdiff(names(formals()),
        "..."))
    for (exclusion in exclusions)
        variableList[[exclusion]] = NULL

    if (length(variableList) == 0)
        stop("Must supply reference object.")

    if (expandCharacter) {
        temp = character()
        for (variable in variableList) {
            if (typeof(variable) == "character")
                temp <- c(temp, variable)
            else if (typeof(variable) == "symbol") {
                evaluatedVariable <- eval(variable)
                if (typeof(evaluatedVariable) == "character")
                    temp <- c(temp, evaluatedVariable)
                else if (is.environment(evaluatedVariable)) {
                    for (name in ls(evaluatedVariable))
                        temp <- c(temp, variable %_% "$" %_% name)
                }
                else
                    temp <- c(temp, as.character(variable))
            }
        }
        pointerNames <- temp
    }
    else
        pointerNames <- as.character(variableList)

    returnList <- list()
    for (pointerName in pointerNames) {
        e <- envir
        pName <- pointerName

        reEnv <- "^(.+?)\\$(.+?)$"
        envMatch <- regexec(reEnv, pointerName)
        envMatches <- NULL
        if (envMatch[[1]][1] != -1) {
            envMatches <- regmatches(pointerName, envMatch)[[1]][2:3]
            e <- get(envMatches[1])
            pName <- envMatches[2]
        }

        p <- list()
        p$object <- e
        p$name <- as.character(pName)
        class(p) <- "pointer"

        index <- length(returnList) + 1
        if (namedList) index <- p$name

        returnList[[index]] <- p
    }

    if (length(returnList) == 1)
        return (returnList[[1]])

    return (returnList)
}

as.pointer <- function(x)
{
    pointer(x)
}

is.pointer <- function(x)
{
    return (inherits(x, "pointer"))
}

.. <- deref <- function(x)
{
    if (is.environment(x)) return (x)
    else return (get(x$name, envir=x$object))
}
`..<-` <- `deref<-` <- function(x, value)
{
    if (is.pointer(x)) assign(x$name, value, envir=x$object)
    return (x)
}

print.pointer <- function(x, ...)
{
    environment.name <- capture.output(print(x$object))
    cat("Pointer to variable '", x$name, "' in ", environment.name, ":\n\n", sep="")
    str(..(x), ...)
}

## usage:
# x <- list(frog="frog", fish="~frog")
# z <- pointer(x)
# ..(z)
# ..(z)$fish <- "trout"
# ..(z)
# x





.getmetadata <- function(path, pkgname)
{
    if (is(path, "Hub")){
        db_path <- AnnotationHub::dbfile(path)
    } else if (class(path) == "character"){
        db_path <- path
    }else{
        db_path <- RSQLite::dbGetInfo(path)$dbname
    }
    query <- paste0(
        "SELECT DISTINCT resources.ah_id, resources.title, resources.description, biocversions.biocversion, resources.species, resources.genome, resources.taxonomyid, input_sources.sourcetype, input_sources.sourceurl, input_sources.sourceversion, resources.coordinate_1_based, resources.dataprovider, resources.maintainer, rdatapaths.rdataclass, rdatapaths.dispatchclass, location_prefixes.location_prefix, rdatapaths.rdatapath, tags.tag FROM resources, rdatapaths, biocversions, input_sources, location_prefixes, tags WHERE resources.id == rdatapaths.resource_id AND resources.location_prefix_id == location_prefixes.id AND biocversions.resource_id == resources.id AND input_sources.resource_id == resources.id AND tags.resource_id == resources.id AND resources.preparerclass == '",pkgname,"'")
    if (is(path, "Hub")){
        conn <- AnnotationHub::dbfile(path)
    } else if (class(path) == "character"){
        conn <- AnnotationHub:::.db_open(path)
        on.exit(AnnotationHub:::.db_close(conn))
    }else{
        conn <- path
    }
    mat <- AnnotationHub:::.db_query(conn, query)

    ## now fix tags -- tags format is ":" seperated but in database each unique
    ## column results in multiple rows
    for(vl in unique(mat[,"ah_id"])){
        dx = which(mat[,"ah_id"]==vl)
        mat[dx,"tag"] = paste0(mat[dx,"tag"], collapse=":")
    }
    mat = unique(mat)
    ## add hubid column for base resource
    mat = cbind(hubid = gsub('\\..*', '',mat$ah_id), mat)
    ## fix colnames to be correct case for valid metadata
    colnames(mat) = c("HubId", "ah_id", "Title", "Description", "BiocVersion",
                      "Species", "Genome", "TaxonomyId", "SourceType", "SourceUrl",
                      "SourceVersion", "Coordinate_1_based", "DataProvider",
                      "Maintainer", "RDataClass", "DispatchClass",
                      "Location_Prefix", "RDataPath", "Tags")
    mat
}
