### =========================================================================
### HubMetadata objects
### -------------------------------------------------------------------------
###

setOldClass(c("POSIXct", "POSIXt"))
setOldClass("numeric_version")
setOldClass(c("package_version", "numeric_version"))

## The prototype needs to be fully specified, using 'NA' to indicate
## unknown, otherwise to / from JSON is confused
setClass("HubMetadata",
    representation(
        "VIRTUAL",
        HubRoot="character",
        BiocVersion="package_version",
        Coordinate_1_based="logical",
        DataProvider="character",
        DerivedMd5="character",
        Description='character',
        Genome="character",                 ## needed for record_id
        Maintainer="character",
        Notes='character',
        RDataClass="character",             ## needed for record_id
        RDataDateAdded="POSIXct",
        RDataPath="character",
        Recipe="character",                 ## no longer needed for record_id
        SourceLastModifiedDate="POSIXct",
        SourceMd5="character",
        SourceSize="numeric",
        SourceUrl="character",              ## needed for record_id
        SourceVersion="character",
        SourceType="character",
        Species="character",
        Tags='character',
        TaxonomyId="integer",               ## needed for record_id
        Title="character",
        Location_Prefix="character",
        DispatchClass="character",
        PreparerClass="character",          ## needed for record_id
        HubId="character",
        Error="character"
    ),
    prototype = prototype(
        HubRoot=NA_character_,
        BiocVersion=BiocManager::version(),
        Coordinate_1_based=NA,
        DataProvider=NA_character_,
        DerivedMd5=NA_character_,
        Description=NA_character_,
        Genome=NA_character_,
        Maintainer=
            "Bioconductor Package Maintainer <maintainer@bioconductor.org>",
        Notes=NA_character_,
        RDataClass=NA_character_,
        RDataDateAdded=as.POSIXct(NA_character_),
        RDataPath=NA_character_,
        Recipe=NA_character_,
        SourceLastModifiedDate=as.POSIXct(NA_character_),
        SourceMd5=NA_character_,
        SourceSize=NA_real_,
        SourceVersion=NA_character_,
        SourceType=NA_character_,
        Species=NA_character_,
        Tags=NA_character_,
        TaxonomyId=NA_integer_,
        Title=NA_character_,
        Location_Prefix=NA_character_,
        DispatchClass=NA_character_,
        PreparerClass=NA_character_,
        HubId=NA_character_,
        Error="NA_character"
    )
)

## ----------------------------------------------------------------------------
## generics
##

setGeneric("recipeName", signature="object",
    function(object) standardGeneric ("recipeName")
)

setGeneric("inputFiles", signature="object",
           function(object, ...) standardGeneric ("inputFiles")
)

setGeneric("outputFile", signature="object",
           function(object) standardGeneric ("outputFile")
)

setGeneric("run", signature="object",
    function(object, recipeFunction, ...) standardGeneric ("run")
)

setGeneric("hubError", function(x) standardGeneric("hubError"))

setGeneric("hubError<-", signature=c("x", "value"),
    function(x, value) standardGeneric("hubError<-")
)

## ------------------------------------------------------------------------------
## getters and setters
##

setMethod("metadata", "HubMetadata",
    function(x, ...) {
        nms <- slotNames(class(x))
        names(nms) <- nms
        lapply(nms, slot, object=x)
    }
)

setReplaceMethod("metadata", c("HubMetadata", "list"),
     function(x, ..., value)
         do.call(new, c(class(x), x, value))
)

setMethod("recipeName", "HubMetadata",
    function(object)
        metadata(object)$Recipe
)

setMethod("inputFiles", "HubMetadata",
    function(object, useRoot=TRUE) {
        if(useRoot==TRUE){
            res <- file.path(metadata(object)$HubRoot,
                             metadata(object)$RDataPath)
        }else{
            res <- metadata(object)$SourceUrl
        }
        res
    }
)

setMethod("outputFile", "HubMetadata",
    function(object)
        file.path(metadata(object)$HubRoot,
                  basename(metadata(object)$RDataPath))
)

setMethod("hubError", "HubMetadata",
    function(x) x@Error
)

setMethod("hubError", "list",
    function(x)
    {
        if (!all(sapply(x, is, "HubMetadata")))
            stop("all elements of 'value' must be 'HubMetadata' objects")
        sapply(x, hubError)
    }
)

setReplaceMethod("hubError", c("HubMetadata", "character"),
    function(x, value)
    {
        x@Error <- value
        x
    }
)

setReplaceMethod("hubError", c("list", "character"),
    function(x, value)
    {
        if (!all(sapply(x, is, "HubMetadata")))
            stop("all elements of 'x' must be 'HubMetadata' objects")
        lapply(x, "hubError<-", value=value)
    }
)

## ------------------------------------------------------------------------------
## show
##

setMethod(show, "HubMetadata",
    function(object)
{
    cat("class: ", class(object), '\n', sep='')
    for (slt in sort(slotNames(object))) {
        value <- slot(object, slt)
        txt <- paste0(slt, ": ", paste0(as.character(value), collapse=" "))
        cat(strwrap(txt), sep="\n  ")
    }
})
