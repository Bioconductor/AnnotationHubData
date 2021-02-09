\name{makeAnnotationHubMetadata}

\alias{makeAnnotationHubMetadata}

\title{
  Make AnnotationHubMetadata objects from csv file of metadata
}

\description{
  Make AnnotationHubMetadata objects from .csv files located in the
  "inst/extdata/" package directory of an AnnotationHub package.
}


\usage{
  makeAnnotationHubMetadata(pathToPackage, fileName=character())
}

\arguments{
  \item{pathToPackage}{
    Full path to data package including the package name; no trailing slash
  }
  \item{fileName}{
    Name of metadata file(s) with csv extension. If none are provided, all
    files with .csv extension in "inst/extdata" will be processed.
  }
}

\details{
  \itemize{
    \item{makeAnnotationHubMetadata:}{
      Reads the resource metadata from .csv files into a
      \link{AnnotationHubMetadata} object. The \link{AnnotationHubMetadata}
      is inserted in the AnnotationHub database. Intended for internal
      use or package authors checking the validity of package metadata.
    }
  }

  \itemize{
    \item{Formatting metadata files:}{

      \code{makeAnnotationHubMetadata} reads .csv files of metadata
      located in "inst/extdata". Internal functions perform checks for
      required columns and data types and can be used by package authors
      to validate their metadata before submitting the package for
      review.

      The rows of the .csv file(s) represent individual \code{Hub}
      resources (i.e., data objects) and the columns are the metadata
      fields. All fields should be a single character string of length 1.

      Required Fields in metadata file:
      \itemize{
	\item Title: \code{character(1)}. Name of the resource. This can be
	      the exact file name (if self-describing) or a more complete
	      description.

	\item Description: \code{character(1)}. Brief description of the
	      resource, similar to the 'Description' field in a package
	      DESCRIPTION file.

	\item BiocVersion: \code{character(1)}. The first Bioconductor version
	      the resource was made available for. Unless removed from
	      the hub, the resource will be available for all versions
	      greater than or equal to this field. Generally the current
	      devel version of Bioconductor.

	\item Genome: \code{character(1)}. Genome. Can be NA.

	\item SourceType: \code{character(1)}. Format of original data, e.g., FASTA,
	BAM, BigWig, etc. \code{getValidSourceTypes()} list currently
	acceptable values. If nothing seems appropiate for your data
	reach out to maintainer@bioconductor.org.

	\item SourceUrl: \code{character(1)}. Optional location of original
	      data files. Multiple urls should be provided as a comma separated
	      string.

	\item SourceVersion: \code{character(1)}. Version of original data.

	\item Species: \code{character(1)}. Species. For help on valid
	species see \code{getSpeciesList, validSpecies, or
	suggestSpecies. Can be NA.}

	\item TaxonomyId: \code{character(1)}. Taxonomy ID. There are
	checks for valid taxonomyId given the Species which produce
	warnings. See GenomeInfoDb::loadTaxonomyDb() for full validation
	table. Can be NA.

	\item Coordinate_1_based: \code{logical}. TRUE if data are
	1-based. Can be NA

	\item DataProvider: \code{character(1)}. Name of company or institution
	      that supplied the original (raw) data.

	\item Maintainer: \code{character(1)}. Maintainer name and email in the
	      following format: Maintainer Name <username@address>.

	\item RDataClass: \code{character(1)}. R / Bioconductor class the data
	      are stored in, e.g., GRanges, SummarizedExperiment,
	      ExpressionSet etc. If the file is loaded or read into R
	      what is the class of the object.
	      
	\item DispatchClass: \code{character(1)}. Determines how data are
	      loaded into R. The value for this field should be
	      \sQuote{Rda} if the data were serialized with \code{save()} and
	      \sQuote{Rds} if serialized with \code{saveRDS}. The filename
	      should have the appropriate \sQuote{rda} or \sQuote{rds}
	      extension. There are other available DispathClass types
	      and the function \code{AnnotationHub::DispatchClassList()}
	      
	      A number of dispatch classes are pre-defined in
	      AnnotationHub/R/AnnotationHubResource-class.R with the suffix
	      \sQuote{Resource}. For example, if you have sqlite files, the
	      AnnotationHubResource-class.R defines SQLiteFileResource so
	      the DispatchClass would be SQLiteFile. Contact
	      maintainer@bioconductor.org if you are not sure which class
	      to use. The function
	      \code{AnnotationHub::DispatchClassList()} will output a
	      matrix of currently implemented DispatchClass and brief
	      description of utility. If a predefine class does not seem
	      appropriate contact maintainer@bioconductor.org.  An all
	      purpose DispathClass is \code{FilePath} that instead of trying
	      to load the file into R, will only return the path to the
	      locally downloaded file.
	      
	\item Location_Prefix: \code{character(1)}. Do not include this field
	      if data are stored in the Bioconductor AWS S3; it will be
	      generated automatically.

	      If data will be accessed from a location other than AWS S3
	      this field should be the base url.

	\item RDataPath: \code{character()}.This field should be the
	      remainder of the path to the resource. The
	      \code{Location_Prefix} will be prepended to
	      \code{RDataPath} for the full path to the resource.
	      If the resource is stored in Bioconductor's AWS S3
	      buckets, it should start with the name of the package associated
	      with the metadata and should not start with a leading
	      slash. It should include the resource file name. For
	      strongly associated files, like a bam file and its index
	      file, the two files should be separates with a colon
	      \code{:}. This will link a single hub id with the multiple files.

       \item Tags: \code{character() vector}.
	     \sQuote{Tags} are search terms used to define a subset of
	     resources in a \code{Hub} object, e.g, in a call to \code{query}.

	     \sQuote{Tags} are automatically generated from the
	     \sQuote{biocViews} in the DESCRIPTION and applied to all
	     resources of the metadata file. Optionally, maintainers can
	     define \sQuote{Tags} column of the metadata to define tags
	     for each resource individually. Multiple \sQuote{Tags} are
	     specified as a colon separated string, e.g., tags for two
	     resources would look like this:

	     \preformatted{
	     Tags=c("tag1:tag2:tag3", "tag1:tag3")
	     }



      }
      NOTE: The metadata file can have additional columns beyond the 'Required
      Fields' listed above. These values are not added to the Hub database but
      they can be used in package functions to provide an additional level of
      metadata on the resources.

      More on \code{Location_Prefix} and \code{RDataPath}. These two fields make up
      the complete file path url for downloading the data file. If using
      the Bioconductor AWS S3 bucket the Location_Prefix should not be
      included in the metadata file[s] as this field will be populated
      automatically.  The \code{RDataPath} will be the directory structure you
      uploaded to S3. If you uploaded a directory \sQuote{MyAnnotation/}, and
      that directory had a subdirectory \sQuote{v1/} that contained two files
      \sQuote{counts.rds} and \sQuote{coldata.rds}, your metadata file will contain
      two rows and the RDataPaths would be \sQuote{MyAnnotation/v1/counts.rds}
      and \sQuote{MyAnnotation/v1/coldata.rds}.  If you host your data on a
      publicly accessible site you must include a base url as the
      \code{Location_Prefix}. If your data file was at
      \sQuote{ftp://myinstiututeserver/biostats/project2/counts.rds}, your
      metadata file will have one row and the \code{Location_Prefix} would be
      \sQuote{ftp://myinstiututeserver/} and the \code{RDataPath} would be
      \sQuote{biostats/project2/counts.rds}.
      
    }
  }
}

\value{
    A named list the length of \code{fileName}. Each element is a list of
    of \code{AnnotationHubMetadata} objects created from the .csv file.
}

\seealso{
  \itemize{
    \item \code{\link{updateResources}}
    \item \code{\link{AnnotationHubMetadata}} class
  }
}

\examples{

## Each row of the metadata file represents a resource added to one of
## the 'Hubs'. This example creates a metadata.csv file for a single resource.
## In the case of multiple resources, the arguments below would be character
## vectors that produced multiple rows in the data.frame.

meta <- data.frame(
    Title = "RNA-Sequencing dataset from study XYZ",
    Description = paste0("RNA-seq data from study XYZ containing 10 normal ",
			 "and 10 tumor samples represented as a",
			 "SummarizedExperiment"),
    BiocVersion = "3.4",
    Genome = "GRCh38",
    SourceType = "BAM",
    SourceUrl = "http://www.path/to/original/data/file",
    SourceVersion = "Jan 01 2016",
    Species = "Homo sapiens",
    TaxonomyId = 9606,
    Coordinate_1_based = TRUE,
    DataProvider = "GEO",
    Maintainer = "Your Name <youremail@provider.com>",
    RDataClass = "SummarizedExperiment",
    DispatchClass = "Rda",
    ResourceName = "FileName.rda"
)

\dontrun{
## Write the data out and put in the inst/extdata directory.
write.csv(meta, file="metadata.csv", row.names=FALSE)

## Test the validity of metadata.csv
makeAnnotationHubMetadata("path/to/mypackage")
}
}

\keyword{methods}
