initialTimeout <- getOption("timeout")
setup <- function()
    options(timeout=5*60)
tearDown <- function()
    options(timeout=initialTimeout)

.httrRead <- AnnotationHubData:::.httrRead
.ftpDirectoryInfo <- AnnotationHubData:::.ftpDirectoryInfo
.ftpFileInfo <- AnnotationHubData:::.ftpFileInfo 
.listRemoteFiles <- AnnotationHubData:::.listRemoteFiles
.getGenomeAbbrevs <- AnnotationHubData:::.getGenomeAbbrevs

test_httrRead <- function() {
    setup()
    hg19Url <- "http://hgdownload.cse.ucsc.edu/goldenpath/hg19/encodeDCC/"
    url <- paste0(hg19Url,"wgEncodeCshlLongRnaSeq/")
    ans <- .httrRead(url, "//pre/a/text()")
    checkTrue(is(ans, "data.frame"))
    checkTrue(names(ans) == "files")
    checkTrue(nrow(ans) > 0)
    tearDown()
}

test_ftpDirectoryInfo <- function(){
    setup()
    url <- "ftp://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_mouse/release_M6/"
    ans <- .ftpDirectoryInfo(url)
    checkTrue(is(ans, "character"))
    checkTrue(is.null(names(ans)))
    checkTrue(length(ans) > 0L)
    tearDown()
}

test_ftpFileInfo <- function(){
    setup()
    url <- "ftp://ftp.ensembl.org/pub/release-98/gtf/homo_sapiens/"
    ans <- .ftpFileInfo(url, "chr.gtf.gz")
    checkTrue(is(ans, "data.frame"))
    checkIdentical(names(ans), c("fileurl", "date", "size"))
    checkTrue(nrow(ans) == 1L)

    ans <- .ftpFileInfo(url, ".gz")
    checkTrue(nrow(ans) > 0L)

    ans <- .ftpFileInfo(url, "FILE_THAT_DOESNT_EXIST.gz")
    checkTrue(is(ans, "data.frame"))
    checkIdentical(names(ans), c("fileurl", "date", "size"))
    checkTrue(nrow(ans) == 0L)
    tearDown()
}

test_listRemoteFiles <- function(){
    setup()
    url <- "ftp://ftp.ensembl.org/pub/"
    ans <- .listRemoteFiles(url)
    checkTrue(is(ans, "character"))
    checkTrue(is.null(names(ans)))
    checkTrue(length(ans) > 0L)
    tearDown()
}

## FIXME: revisit this when working on UCSCTrackUpdateChecker.R
test_getGenomeAbbrevs <- function(){
  smallSample <- c("hg19", "hg18", "hg17")
  actualResult <- .getGenomeAbbrevs(smallSample)
  expectedResult <- sort(smallSample)
  checkEquals(actualResult, expectedResult)

  # Viewing the FTP server content at ftp://hgdownload.cse.ucsc.edu/goldenPath/ , you'll
  # notice that some files are actually symlinks to other directories.  Since the
  # getGenomeAbbrevs function claims to handle symlinks, we must test that specific case.
  # The following are files that actually render the client a symlink. :
  ### cb1 -> cbJul2002
  ### ce1 -> ceMay2003
  ### hg15 -> 10april2003/"
  ### rn2 -> rnJan2003
  # You should notice, however that results are returned in a sorted order.
  sampleWithSymlink <- c("hg15", "cb1", "rn2", "ce1")
  resultWithSymlink <- AnnotationHubData:::.getGenomeAbbrevs(sampleWithSymlink)
  expectedResult <- sort(sampleWithSymlink)
  checkEquals(resultWithSymlink, expectedResult)
}
