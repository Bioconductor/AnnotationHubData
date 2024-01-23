### =========================================================================
### makeNCBIToOrgDbs ('non-standard' OrgDbs)
### -------------------------------------------------------------------------
###

## This recipe makes 'non-standard' OrgDb sqlite files from data
## at ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/. These OrgDbs are less
## comprehensive than the 'standard' packages available in the
## Bioconductor repo. This code generates 1000 sqlite files.

## This recipe should be run right before a new release. The
## biocversion should be the current devel version, soon to roll over
## to the new release.

.loadSpecData <- function() {
    dataenv <- new.env(parent = emptyenv())
    data("specData", package = "GenomeInfoDbData", envir = dataenv)
    specData <- dataenv[["specData"]]
    specData[!is.na(specData[["species"]]), ]
}

.loadResults <- function() {
    viablefile <- system.file(
        'extdata', 'viableIDs.rda', package = 'AnnotationForge', mustWork = TRUE
    )
    dataenv <- new.env(parent = emptyenv())
    load(file = viablefile, envir = dataenv)
    dataenv[["results"]]
}

## The 'standard' OrgDbs are generated with makeStandardOrgDbsToSqlite.R.

.NCBIMetadataFromUrl <- function(baseUrl, justRunUnitTest, biocVersion, currentMetadata) {
    ids <- .loadResults()
    if (justRunUnitTest) ids <- head(ids)
    ## FIXME: need different solution; this subset produces NAs
    if (length(biocVersion) > 1) {
        stop(paste("'biocVersion' must be a single value. Make sure new",
                   "'OrgDbs' go into the CORRECT Bioconductor version!"))
    }
    ## Marc's note:
    ## need to find an alternative to this... old school table of tax Ids
    sd <- .loadSpecData()
    ## need to find offenders
    lookup <- function(id){
        message(paste0("looking up value for: ", id))
        GenomeInfoDb:::lookup_organism_by_tax_id(id, all=TRUE)
    }
    ## Some taxonomy IDs cannot be looked up at all - so discard
    ids <- as.numeric(ids[ids %in% sd$tax_id])
    res <- lapply(ids,lookup)
    taxonomyId <-
        as.integer(as.character(unlist(lapply(res, function(x){x$tax_id}))))
    genus <- unlist(lapply(res, function(x){x$genus}))
    species <- unlist(lapply(res, function(x){x$species}))
    genus <- gsub(" ", "_", genus)
    genus <- gsub("/", "|", genus)
    species <- gsub(" ", "_", species)
    species <- gsub("/", "|", species)

    oriSpecies <- paste(genus, species)
    fullSpecies <- gsub(" ", "_", oriSpecies)

    title <- paste0("org.", fullSpecies, ".eg", ".sqlite")
    rDataPath <- paste0("ncbi/uniprot/",biocVersion,"/",title)

    genome <- setNames(rep("NCBI genomes", length(fullSpecies)), title)
    dateMessage <- paste0('NCBI gene annotations as of ', as.character(date()))
    sourceVersion <- rep(dateMessage, length(fullSpecies))
    description <- paste("NCBI gene ID based annotations about", oriSpecies)
    sourceUrls <- c(baseUrl,"ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/idmapping_selected.tab.gz")
    sourceUrl <- rep(list(sourceUrls), length(fullSpecies))

    tryCatch({
        if (!requireNamespace("AzureStor", quietly = TRUE)){
            message("AzureStor not installed.\n  Regenerating all files")
            azurefiles <- character(0)
        }else{
            sas = Sys.getenv("AZURE_SAS_TOKEN", NA_character_)
            if(is.na(sas)){
                message("AZURE_SAS_TOKEN environment variable is not set.\n  Regenerating all files.")
                azurefiles <- character(0)
            }else{
                ep <- AzureStor::storage_endpoint(endpoint="https://bioconductorhubs.blob.core.windows.net",
                                                  sas=sas)
                ## assumes upload to staginghub
                container <- AzureStor::storage_container(ep, "staginghub")
                azurefiles <- AzureStor::list_storage_files(container,
                                         basename(currentMetadata$AnnotationHubRoot))[,"name"]
                azurefiles <- unlist(lapply(azurefiles, FUN=basename))
            }
        }
    }, error=function(e){
        azurefiles <- character(0)
    }, finally={
        if (!exists("azurefiles"))  azurefiles <- character(0)
    })

    if (length(azurefiles) != 0){
        subset <- !(title %in% azurefiles)
        if(any(subset)){
            lst <- lapply(list(title=title, species = oriSpecies,
                               taxonomyId = taxonomyId, genome = genome, sourceUrl=sourceUrl,
                               sourceVersion = sourceVersion,
                               description=description, rDataPath=rDataPath), "[", subset)
        }else{
            lst <- list(title=title, species = oriSpecies,
                        taxonomyId = taxonomyId, genome = genome, sourceUrl=sourceUrl,
                        sourceVersion = sourceVersion,
                        description=description, rDataPath=rDataPath)
        }

    }else{

        lst <- list(title=title, species = oriSpecies,
                    taxonomyId = taxonomyId, genome = genome, sourceUrl=sourceUrl,
                    sourceVersion = sourceVersion,
                    description=description, rDataPath=rDataPath)
    }
    lst
}


needToRerunNonStandardOrgDb <- function(biocVersion =  BiocManager::version(),
                                        baseUrl = "ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/",
                                        resourceDir=".", justRunUnitTest=FALSE){
    ids <- .loadResults()
    if (justRunUnitTest) ids <- head(ids)
    ## FIXME: need different solution; this subset produces NAs
    if (length(biocVersion) > 1) {
        stop(paste("'biocVersion' must be a single value. Make sure new",
                   "'OrgDbs' go into the CORRECT Bioconductor version!"))
    }
    ## Marc's note:
    ## need to find an alternative to this... old school table of tax Ids
    sd <- .loadSpecData()
    ## need to find offenders
    lookup <- function(id){
        message(paste0("looking up value for: ", id))
        GenomeInfoDb:::lookup_organism_by_tax_id(id, all=TRUE)
    }
    ## Some taxonomy IDs cannot be looked up at all - so discard
    ids <- as.numeric(ids[ids %in% sd$tax_id])
    res <- lapply(ids,lookup)
    taxonomyId <-
        as.integer(as.character(unlist(lapply(res, function(x){x$tax_id}))))
    genus <- unlist(lapply(res, function(x){x$genus}))
    species <- unlist(lapply(res, function(x){x$species}))
    genus <- gsub(" ", "_", genus)
    genus <- gsub("/", "|", genus)
    species <- gsub(" ", "_", species)
    species <- gsub("/", "|", species)

    oriSpecies <- paste(genus, species)
    fullSpecies <- gsub(" ", "_", oriSpecies)

    title <- paste0("org.", fullSpecies, ".eg", ".sqlite")

    tryCatch({
        if (!requireNamespace("AzureStor", quietly = TRUE)){
            message("AzureStor not installed.\n  Cannot determine.")
            azurefiles <- character(0)
        }else{
            sas = Sys.getenv("AZURE_SAS_TOKEN", NA_character_)
            if(is.na(sas)){
                message("AZURE_SAS_TOKEN environment variable is not set.\n  Cannot determine.")
                azurefiles <- character(0)
            }else{
                ep <- AzureStor::storage_endpoint(endpoint="https://bioconductorhubs.blob.core.windows.net",
                                                  sas=sas)
                ## assumes upload to staginghub
                container <- AzureStor::storage_container(ep, "staginghub")
                azurefiles <- AzureStor::list_storage_files(container, resourceDir)[,"name"]
                azurefiles <- unlist(lapply(azurefiles, FUN=basename))
            }
        }
    }, error=function(e){
        azurefiles <- character(0)
    }, finally={
        if (!exists("azurefiles"))  azurefiles <- character(0)
    })

    if (length(azurefiles)){
        subset <- !(title %in% azurefiles)
        res <- any(subset)
    }else{
        res <- TRUE
    }
    res
}



oldAWSS3_needToRerunNonStandardOrgDb <- function(biocVersion =  BiocManager::version(),
                                        baseUrl =
                                        "ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/"){
    ids <- .loadResults()
    ## FIXME: need different solution; this subset produces NAs
    if (length(biocVersion) > 1) {
        stop(paste("'biocVersion' must be a single value. Make sure new",
                   "'OrgDbs' go into the CORRECT Bioconductor version!"))
    }
    ## Marc's note:
    ## need to find an alternative to this... old school table of tax Ids
    sd <- .loadSpecData()
    ## need to find offenders
    lookup <- function(id){
        message(paste0("looking up value for: ", id))
        GenomeInfoDb:::lookup_organism_by_tax_id(id, all=TRUE)
    }
    ## Some taxonomy IDs cannot be looked up at all - so discard
    ids <- as.numeric(ids[ids %in% sd$tax_id])
    res <- lapply(ids,lookup)
    taxonomyId <-
        as.integer(as.character(unlist(lapply(res, function(x){x$tax_id}))))
    genus <- unlist(lapply(res, function(x){x$genus}))
    species <- unlist(lapply(res, function(x){x$species}))
    genus <- gsub(" ", "_", genus)
    genus <- gsub("/", "|", genus)
    species <- gsub(" ", "_", species)
    species <- gsub("/", "|", species)

    oriSpecies <- paste(genus, species)
    fullSpecies <- gsub(" ", "_", oriSpecies)

    title <- paste0("org.", fullSpecies, ".eg", ".sqlite")

    tryCatch({
        aws <- system2("aws",
                       args=paste0("s3 ls s3://annotationhub/ncbi/uniprot/",
                           biocVersion," --recursive"), stdout=TRUE, stderr=TRUE)
    }, error=function(e){
        aws <- character(0)
        stop("Cannot access AWS. Unable to determine")
    }, finally={
        if (!exists("aws"))  aws <- character(0)
        aws <- gsub("\\s+", " ", stringr::str_trim(aws))
        aws <- aws[-1]
    })

    if (length(aws)){
        s3titles <- sapply(strsplit(sapply(strsplit(aws, " "),"[[", 4), "/"),"[[",4)
        subset <- !(title %in% s3titles)
        res <- any(subset)
    }else{
        res <- TRUE
    }
    res
}

## STEP 1: make function to process metadata into AHMs
makeNCBIToOrgDbsToAHM <-
    function(currentMetadata, justRunUnitTest = FALSE,
             BiocVersion =  BiocManager::version(),
             baseUrl = "ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/") {
    meta <- .NCBIMetadataFromUrl(baseUrl, justRunUnitTest,
                                 biocVersion=BiocVersion[[1]],
                                 currentMetadata)

    message("Processing ", length(meta[[1]]), " files.")

    Map(AnnotationHubMetadata,
        Description=meta$description,
        Genome=meta$genome,
        SourceUrl=meta$sourceUrl,
        SourceVersion=meta$sourceVersion,
        Species=meta$species,
        TaxonomyId=meta$taxonomyId,
        Title=meta$title,
        RDataPath=meta$rDataPath,
        MoreArgs=c(currentMetadata, list(
            BiocVersion=package_version(BiocVersion),
            Coordinate_1_based = TRUE,
            DataProvider = baseUrl,
            Maintainer = paste("Bioconductor Package Maintainer",
                               "<maintainer@bioconductor.org>"),
            RDataClass = "OrgDb",
            DispatchClass = "SQLiteFile",
            SourceType="NCBI/UniProt",
            RDataDateAdded = Sys.time(),
            Recipe = "AnnotationHubData:::NCBIToOrgDbs",
            Tags = c("NCBI", "Gene", "Annotation"))))
}

## STEP 2: Make a recipe function that takes an AnnotationHubRecipe object.
NCBIToOrgDbs <- function(ahm){
    fullSpecies <- ahm@Species
    genus <- unlist(strsplit(fullSpecies,split=" "))[1]
    species <- unlist(strsplit(fullSpecies,split=" "))[2]
    dbname <- makeOrgPackageFromNCBI(version="1.0.0",
                                     maintainer=ahm@Maintainer,
                                     author=ahm@Maintainer,
                                     outputDir=dirname(ahm@HubRoot),
                                     tax_id=as.character(ahm@TaxonomyId),
                                     genus=genus,
                                     species=species,
                                     NCBIFilesDir=dirname(ahm@HubRoot),
                                     databaseOnly=TRUE,
                                     rebuildCache=TRUE)
    file.rename(from=file.path(dirname(ahm@HubRoot), dbname), to=file.path(ahm@HubRoot, ahm@RDataPath))
    file.path(ahm@HubRoot, ahm@RDataPath)
}

## STEP 3: Call the helper to set up the newResources() method
makeAnnotationHubResource("NCBIImportPreparer",
                          makeNCBIToOrgDbsToAHM)
