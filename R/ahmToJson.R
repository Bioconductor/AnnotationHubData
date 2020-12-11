## Code for creating json records from the sqlite DB.

## RIGHT NOW the json looks sort of record centric.  So I need to make
## code that takes AHMs and makes them into JSON.


## So 1st I need an exemplar AHM (will save one for now in inst/extdata

## So for testing: 
## load(system.file('extdata','inpDrosPsuedo.rda', package='AnnotationHubData'))
## ahm


## helper to do cleanup and make sure things are present:
cleanupLst <- function(lst){
    if(is.na(lst[["recipe"]])){ return(lst) }
    
    if(length(lst[["recipe"]])==1){
        lst[["recipe"]][[2]] <- "AnnotationHubData"
    }
    ## Unfortunately, I have no recipe args (so I can't fix that field)
    ## But I DO have this translation file Dan made me...
    ## looks like there are some issues with the data..
    if(lst[["recipe"]][1]=='extendedBedToGRanges'){
        file <- system.file('extdata','titlesToRecipes.txt',
                            package='AnnotationHubData')
        trns <- read.delim(file, header=FALSE, stringsAsFactors=FALSE)
        idx <- trns[[2]] %in% lst[["sourceurl"]]
        value <- trns[idx,][[1]]
        if(length(value)==1){
            lst[["recipe"]][1] <- value
        }else{
           warning("no matching value for recipe called 'extendedBedToGRanges'")
        }
    }
    lst
}



## Dan suggests jsonlite
ahmToJson <- function(ahm){
    lst <- metadata(ahm)    
    
    ## casting on elements that toJSON can't handle
    lst[['BiocVersion']] <- as.character(lst[['BiocVersion']])
    lst[['SourceLastModifiedDate']] <- as.character(lst[['SourceLastModifiedDate']])
    ## lower case all the names
    names(lst) <- tolower(names(lst))
    
    ##TEMP cleanup the ahm (in future we want to stop using this!)
    lst <- cleanupLst(lst)
    
    rdatapaths <- Map(list,
                      rdatapath=lst[['rdatapath']],
                      rdataclass=lst[['rdataclass']],
                      dispatchclass=lst[['dispatchclass']],
                      version_id="null"
                      )
    ## using Map puts unwanted labels on things...
    names(rdatapaths) <- NULL 

    input_sources <- Map(list,
                         sourcesize=lst[['sourcesize']],
                         sourceurl=lst[['sourceurl']],
                         sourcetype=lst[['sourcetype']],
                         sourceversion=lst[['sourceversion']],
                         sourcemd5=lst[['sourcemd5']],
                         sourcelastmodifieddate=lst[['sourcelastmodifieddate']]
                         )
    ## using Map puts unwanted labels on things...
    names(input_sources) <- NULL 
    
    ## TODO: I need to have Map make lists but not have them be named horribly.
    ## So multiplexed like Map on rdatapaths below, but with result
    ## that looks like input_sources
    
    ## Now just need to re-arrange things a bit
    base <- list(title=lst[['title']],
                 dataprovider=lst[['dataprovider']],
                 species=lst[['species']],
                 taxonomyid=as.integer(lst[['taxonomyid']]),
                 genome=lst[['genome']],
                 description=lst[['description']],
                 coordinate_1_based=lst[['coordinate_1_based']],
                 maintainer=lst[['maintainer']],
                 rdataversion=lst[['rdataversion']],
                 rdatadateadded=lst[['rdatadateadded']],
                 ## FIXME - Old AHMs may not have Location_Prefix filled in!
                 ## It should be http://s3.amazonaws.com/annotationhub/
                 ## by default, for chain files it should be:
                 ## http://hgdownload.cse.ucsc.edu/
                 location_prefix=lst[['location_prefix']],
                 recipe=lst[['recipe']][1],
                 recipe_package=ifelse(!is.na(lst[["recipe"]]),
                   lst[['recipe']][2], lst[['recipe']][1] ),
                 rdatapaths=rdatapaths,                 
                 input_sources=input_sources,
                 tags=lst[['tags']],
                 biocversions=lst[['biocversion']],
                 preparerclass=lst[['preparerclass']],
                 ah_id=lst[['hubid']]
                 )
    
    ## then make JSON
    paste0(toJSON(base, auto_unbox=TRUE,na='null', pretty=TRUE), "\n")
    ## STILL: some issues here with no boxing where we want it (around
    ## sub-sets like 'versions'
    ## AND: some name-mangling in the tags...
    
}




## Testing
## numExtends <- unlist(lapply(resources, function(x){x@Recipe[1]=='extendedBedToGRanges'}))


## NOTES from 4/21/14
## check on rdatasize and sourcesize (should not be NA?) - I think
## they are NA though- but double check this. - DONE
## values that are NA in the JSON should be set to null - DONE
## use ALL of the biocversions - DONE
## add sourceMd5, derivedMD5, sourceLastModifiedDate to the json
## (soon) - manually add these to the AHMs? - DONE

## changes to the process for making Annotations:
## Export makeAnnotationHubResource (so it can be used externally in
## other packages) - DONE
## Allow currentMetadata to be passed in to the helper functions (add
## this to
## .generalNewResources::makeAnnotationHubMetadataFunction(currentMEtadata,...)
## - DONE
## Recipes should use require() to minimize dependencies for
## annotations and suggests for things that are only needed by
## specific recipes. Or they could maybe just get away with importing.
## recipes and AHM generator should not have to define an AHMRoot
## (since this is alway put in after the fact. - Just use a default
## value for this. 


## modernize all of the recipes so that they use the new system (the
## new simplified system).


## And actually we now need to also stop defining the AHMRoot this in
## the recipes.  (it is no longer necessary)


## Make sure that we can put a recipe into another package. - untested.


## look into the weird requirement for adding importPreparer subclasses to
## the NAMESPACE. - can we make this go away?


## Document makeAnnotationHubResource


## Fix the unit tests
