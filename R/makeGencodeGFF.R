# recipe to get GFF3 files from Genecode.
# importtant links
#http://www.gencodegenes.org/releases/
#ftp site: ftp://ftp.sanger.ac.uk/pub/gencode/Gencode_human
# readme file for genecode project
# the above code was updated documented at
# ftp://ftp.sanger.ac.uk/pub/gencode/README.txt
#ftp://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/_README.TXT


# for gff3 files
#gencode.vX.annotation.gff3.gz
#gencode.vX.chr_patch_hapl_scaff.annotation.gff3.gz
#gencode.vX.polyAs.gff3.gz:
#gencode.vX.polyAs.gff3.gz:
#gencode.vX.2wayconspseudos.gff3.gz:
# gencode.vX.long_noncoding_RNAs.gff3.gz
#gencode.vX.tRNAs.gff3.gz

# only gff3 files will be added - since both gtf and gff3 contain same
# data, but gff3 is better (Herve) .These files will not be stored as
# a GRanges on amazon s3.

.gencodeBaseUrl <- "ftp://ftp.ebi.ac.uk/pub/databases/gencode/"

.gencodeFileFromUrl <- function(urls) {
    unlist(sapply(urls, function(url) {
        listing <- .ftpDirectoryInfo(url)

        ## find entries marking directory
        idx <- grepl("^./", listing)
        tag <- sub("./(.*):", "\\1/", listing[idx])
        directory <- c("", tag)[cumsum(idx) + 1L]
        ## complete URL
        idx <- grepl("gencode", listing)
        paste0(url, directory, sub(".*gencode", "gencode", listing))[idx]

    }, USE.NAMES=FALSE))
}

.gencodeDescription <- function(fileurls){
    # add description map here.
    map <- c(
      annotation.gff3.gz=.expandLine("Gene annotations
          on reference chromosomes from Gencode"),
      chr_patch_hapl_scaff.annotation.=.expandLine("Gene annotation
          on reference-chromosomes/patches/scaffolds/haplotypes from Gencode"),
      polyAs=.expandLine("files contain polyA signals, polyA sites and
          pseudo polyAs manually annotated by HAVANA from only the refrence
          chromosome"),
      wayconspseudos=.expandLine("pseudogenes predicted by the Yale
          & UCSC pipelines, but not by Havana on reference chromosomes"),
      long_noncoding_RNAs=.expandLine("sub-set of the main annotation files
          on the reference chromosomes. They contain only the lncRNA genes.
          Long non-coding RNA genes are considered the genes with any of
          those biotypes: 'processed_transcript', 'lincRNA',
          '3prime_overlapping_ncrna', 'antisense', 'non_coding',
          'sense_intronic' , 'sense_overlapping' , 'TEC' , 'known_ncrna'."),
      tRNAs =.expandLine("tRNA structures predicted by tRNA-Scan on
          reference chromosomes"),
      transcripts.fa.gz=.expandLine("Protein-coding transcript sequences
          on reference chromosomes Fasta file"),
      translations.fa.gz=.expandLine("Translations of protein-coding
          transcripts on reference chromosomes Fasta file"),
      lncRNA_transcripts.fa.gz=.expandLine("Long non-coding RNA
          transcript sequences on reference chromosomes Fasta file."),
      unmapped=.expandLine("Unmapped")
      )
    description <- character(length(fileurls))
    for (i in seq_along(map))
        description[grep(names(map)[i], fileurls)] <- map[[i]]

    description
}

.gencodeGenome <- function(species, release) {
    # this information is curated from Gencode's website
    # link - http://www.gencodegenes.org/releases/
    if (species=="Human")
      tblurl <- "https://www.gencodegenes.org/human/releases"
    else
      tblurl <- "https://www.gencodegenes.org/mouse/releases"

    ## read in the table
    tryCatch({
        http <- RCurl::getURL(tblurl)
        tbl <- XML::readHTMLTable(http, header=TRUE, stringsAsFactors=FALSE)
    },  error = function(err) {
        stop("Error reading ", tblurl,
    ".\n  SSL issue reported in Ubuntu 20?")
    })
    
    tbl <- tbl[[1]]
    tblheader <- gsub("\n", "", colnames(tbl))
    tblheader = trimws(tblheader)
    colnames(tbl) = tblheader

    idx <- which(tbl[,"GENCODE release"]==release)
    tbl[idx,"Genome assembly version"]
}


# Helper to retrieve GTF & GFF3 file urls from Gencode
.gencodeSourceUrls <- function(species, release, filetype, justRunUnitTest)
{
    speciesUrl <- ifelse(species=="Human", "Gencode_human/", "Gencode_mouse/")
    dirurl = paste0(.gencodeBaseUrl, speciesUrl, "release_", release, "/")
    names(dirurl) <- paste0(species,"_", release)

    fileurls <-.gencodeFileFromUrl(dirurl)

    if (tolower(filetype)=="gff")
       idx <-  grep("gff3", fileurls)
    if(tolower(filetype)=="fasta")
       idx <-  grep("fa.gz", fileurls)
    fileurls <- fileurls[idx]

    if(length(idx)==0)
     stop("No files found.")

     if(justRunUnitTest)
        fileurls <- fileurls[1:2]

    ## tags
    filename <- basename(fileurls)
    filename <- sub(".gz","", filename)
    tags <- gsub("[.]",",",filename)

    ## description
    description <- .gencodeDescription(fileurls)

    ## rdatapath - these files will be made into GRanges and stored on S3.
    #rdatapath <- paste0("gencode/", species, "/release_", release,"/",
    #    basename(fileurls), ".Rda")

    rdatapath <- sub(.gencodeBaseUrl, "", fileurls)


    ## get date and size for files
    df <- .httrFileInfo(fileurls)
    rownames(df) <- NULL

    ## species, taxid, genome
    scSpecies <- ifelse(species=="Human", "Homo sapiens", "Mus musculus")
    taxid <- ifelse(species=="Human", 9606L, 1090L)
    genome <- .gencodeGenome(species, release)
    genome <- rep(genome, length(fileurls))
    genome[grepl('_mapping/', rdatapath)] <-
        gsub('.*/', '',
             gsub('_mapping/.*', '',
                  rdatapath[grepl('_mapping/', rdatapath)])
             )
    scSpecies <- rep(scSpecies, length(fileurls))
    taxid <- rep(taxid, length(fileurls))

    cbind(df, rdatapath, description, tags, species=scSpecies, taxid, genome,
         stringsAsFactors=FALSE)
}


## STEP 1: make function to process metadata into AHMs
makeGencodeGFFsToAHMs <- function(currentMetadata,
                                  species=c("Human", "Mouse"),
                                  release,
                                  justRunUnitTest=FALSE,
                                  BiocVersion=BiocManager::version()){

    ## important - here you need to know which species and release you want to
    ## add files for.
    species <- match.arg(species)
    rsrc <- .gencodeSourceUrls(species = species, release = release,
        filetype = "gff", justRunUnitTest = justRunUnitTest)

    description <- rsrc$description
    title <- basename(rsrc$fileurl)
    genome <- rsrc$genome
    sourceUrls <- rsrc$fileurl
    #
    # FixMe: in .gencodeSourceUrls the data should be LastModified time
    #    in webAccess function .httrFileInfo these urls have that information
    #    in the body not the header but this function is used elsewhere
    #
    sourceVersion <- as.character(rsrc$date) ## should be character
    if(all(is.na(sourceVersion))){
        sourceVersion = rep(release, length(sourceVersion))
    }
    SourceLastModifiedDate <- rsrc$date  ## should be "POSIXct" "POSIXt"
    SourceSize <- as.numeric(rsrc$size)
    tags <- strsplit(rsrc$tag, ",")
    species <- rsrc$species
    rdatapath <- rsrc$rdatapath
    taxid <- rsrc$taxid

    Map(AnnotationHubMetadata,
        Description=description,
        Genome=genome,
        SourceUrl=sourceUrls,
        SourceSize=SourceSize,
        SourceLastModifiedDate=SourceLastModifiedDate,
        SourceVersion=sourceVersion,
        Species=species,
        RDataPath=rdatapath,
        TaxonomyId=taxid,
        Title=title,
        Tags=tags,
        MoreArgs=list(
          BiocVersion=BiocVersion,
          Coordinate_1_based = TRUE,
          DataProvider = "Gencode",
          Maintainer = "Bioconductor Maintainer <maintainer@bioconductor.org>",
          RDataClass = "GRanges",
          DispatchClass="GFF3File",
          SourceType="GFF",
          Location_Prefix=.gencodeBaseUrl,
          RDataDateAdded = Sys.time(),
          Recipe="AnnotationHubData:::gencodeGFFToGRanges"))
}

gencodeGFFToGRanges <- function(ahm)
{
    outputFile(ahm)[[1]]
}

## STEP 2:  Call the helper to set up the newResources() method
makeAnnotationHubResource("GencodeGffImportPreparer",
                          makeGencodeGFFsToAHMs)
