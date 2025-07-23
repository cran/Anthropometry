\name{Anthropometry-package}
\alias{Anthropometry-package}
\docType{package}
\title{
Statistical Methods for Anthropometric Data
}
\description{
Statistical methodologies especially developed to analyze anthropometric data. These methods are aimed at providing effective solutions to some commons problems related to Ergonomics and Anthropometry. They are based on clustering, the statistical concept of data depth, statistical shape analysis and archetypal analysis. Please see Vinue (2017) <doi:10.18637/jss.v077.i06>.
}
\details{
\tabular{ll}{
Package: Anthropometry\cr
Type: Package\cr
Version: 1.20\cr
Date: 2025-07-23\cr
License: GPL (>=2)\cr
LazyLoad: yes\cr
LazyData: yes\cr
}
anthrCases: Helper generic function for obtaining the anthropometric cases.\cr
Anthropometry-internalArchetypoids: Several internal functions to compute and represent archetypes and archetypoids.\cr
Anthropometry-internalHipamAnthropom: Several internal functions used by both HIPAM-MO and HIPAM-IMO algorithms.\cr
Anthropometry-internalPlotTree: Several internal functions used to build the HIPAM plot tree.\cr
Anthropometry-internalTDDclust: Several internal functions to clustering based on the L1 data depth. \cr
archetypesBoundary: Archetypal analysis in multivariate accommodation problem.\cr
archetypoids: Finding archetypoids.\cr
array3Dlandm: Helper function for the 3D landmarks.\cr
bustSizesStandard: Helper function for defining the bust sizes.\cr 
CCbiclustAnthropo: Cheng and Church biclustering algorithm applied to anthropometric data.\cr
cdfDissWomenPrototypes: CDF for the dissimilarities between women and computed medoids and standard prototypes.\cr
checkBranchLocalIMO: Evaluation of the candidate clustering partition in HIPAM-IMO.\cr
checkBranchLocalMO: Evaluation of the candidate clustering partition in HIPAM-MO.\cr
computSizesTrimowa: Computation of the trimowa elements for a given number of sizes defined by the EN.\cr
computSizesHipamAnthropom: Computation of the hipamAnthropom elements for a given number of sizes defined by the EN.\cr
cube8landm: Cube of 8 landmarks.\cr 
cube34landm: Cube of 34 landmarks.\cr
descrDissTrunks: Description of the dissimilarities between women's trunks.\cr
figures8landm: Figures of 8 landmarks with labelled landmarks.\cr
getBestPamsamIMO: Generation of the candidate clustering partition in HIPAM-IMO. \cr
getBestPamsamMO: Generation of the candidate clustering partition in HIPAM-MO.\cr
getDistMatrix: Dissimilarity matrix between individuals and prototypes.\cr
HartiganShapes: Hartigan-Wong k-means for 3D shapes.\cr
hipamAnthropom: HIPAM algorithm for anthropometric data.\cr
landmarksSampleSpaSurv: Landmarks of the sampled women of the Spanish Survey.\cr
LloydShapes: Lloyd k-means for 3D shapes.\cr
nearestToArchetypes: Nearest individuals to archetypes.\cr
optraShapes: Auxiliary optra subroutine of the Hartigan-Wong k-means for 3D shapes.\cr
overlapBiclustersByRows: Overlapped biclusters by rows.\cr
parallelep8landm: Parallelepiped of 8 landmarks.\cr
parallelep34landm: Parallelepiped of 34 landmarks.\cr
percentilsArchetypoid: Helper function for computing percentiles of a certain archetypoid.\cr
plotPrototypes: Prototypes representation.\cr
plotTreeHipamAnthropom: HIPAM dendogram.\cr
plotTrimmOutl: Trimmed or outlier observations representation.\cr
preprocessing: Data preprocessing before computing archetypal observations.\cr
projShapes: Helper function for plotting the shapes.\cr
qtranShapes: Auxiliary qtran subroutine of the Hartigan-Wong k-means for 3D shapes.\cr
sampleSpanishSurvey: Sample database of the Spanish anthropometric survey.\cr
screeArchetypal: Screeplot of archetypal individuals.\cr
shapes3dShapes: 3D shapes plot.\cr
skeletonsArchetypal: Skeleton plot of archetypal individuals.\cr
stepArchetypesRawData: Archetype algorithm to raw data.\cr
stepArchetypoids: Run the archetypoid algorithm several times.\cr
TDDclust: Trimmed clustering based on L1 data depth.\cr
trimmedLloydShapes: Trimmed Lloyd k-means for 3D shapes.\cr
trimmedoid: Trimmed k-medoids algorithm.\cr
trimmOutl: Helper generic function for obtaining the trimmed and outlier observations.\cr
trimowa: Trimmed PAM with OWA operators.\cr
USAFSurvey: USAF 1967 survey.\cr
weightsMixtureUB: Calculation of the weights for the OWA operators.\cr
xyplotPCArchetypes: PC scores for archetypes.\cr
}
\references{
Vinue, G., (2017). Anthropometry: An R Package for Analysis of Anthropometric Data, \emph{Journal of Statistical Software} \bold{77(6)}, 1--39, \doi{10.18637/jss.v077.i06}.
}
\author{
Guillermo Vinue <Guillermo.Vinue@uv.es>, Irene Epifanio, Amelia Simo, M. Victoria Ibanez, Juan Domingo, Guillermo Ayala
}
\keyword{ANTHROP}
