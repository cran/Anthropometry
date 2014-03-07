\name{Anthropometry-package}
\alias{Anthropometry-package}
\docType{package}
\title{
Statistical methods for anthropometric data oriented towards the ergonomic design of products
}
\description{
This package brings together some statistical methodologies especially developed to analyze anthropometric data. These methods are aimed at providing effective solutions to some commons problems related to Ergonomics and Anthropometry. They are based on clustering, the statistical concept of data depth, the statistical shape analysis and the archetypal analysis.
}
\details{
\tabular{ll}{
Package: Anthropometry\cr
Type: Package\cr
Version: 1.0\cr
Date: 2014-03-07\cr
License: GPL-2\cr
LazyLoad: yes\cr
LazyData: yes\cr
}
accommodation: Data preprocessing before computing archetypes.\cr
Anthropometry-internalArchetypoids: Several internal functions to compute and represent archetypes and archetypoids.\cr
Anthropometry-internalDepth: Several internal functions to clustering based on the L1 data depth. \cr
Anthropometry-internalHIPAM: Several internal functions used by both $HIPAM_{MO}$ and $HIPAM_{IMO}$ algorithms.\cr
Anthropometry-internalPlotTree: Several internal functions used to build the HIPAM plot tree.\cr
archetypesBoundary: Archetypal analysis in multivariate accommodation problem.\cr
archetypoids: Finding archetypoids.\cr
CCbiclustAnthropo: Cheng and Church biclustering algorithm applied to anthropometric data.\cr
cdfDiss: CDF for the dissimilarities between women and computed medoids and standard prototypes.\cr
checkBranchLocalIMO: Evaluation of the candidate clustering partition in $HIPAM_{IMO}$.\cr
checkBranchLocalMO: Evaluation of the candidate clustering partition in $HIPAM_{MO}$.\cr
cMDSwomen: Description of the dissimilarities between women's trunks.\cr
compPerc: Computing percentiles of a certain archetypoid.\cr
cube8: Cube of 8 landmarks.\cr 
cube34: Cube of 34 landmarks.\cr
dataDemo: Demo database of the Spanish anthropometric survey.\cr
dataUSAF: USAF 1967 database.\cr
figures8landm: Figures with labelled landmarks.\cr
getBestPamsamIMO: Generation of the candidate clustering partition in $HIPAM_{IMO}$. \cr
getBestPamsamMO: Generation of the candidate clustering partition in $HIPAM_{MO}$.\cr
GetDistMatrix: Dissimilarity matrix between individuals and prototypes.\cr
HartiganShapes: Hartigan-Wong k-means for 3D shapes.\cr
hipamAnthropom: HIPAM algorithm for anthropometric data.\cr
hipamBigGroups: Hipam medoids of the clusters with more than 2 elements.\cr
indivNearest: Nearest individuals to archetypes.\cr
landmarks: Landmarks representing the woman's body.\cr
LloydShapes: Lloyd k-means for 3D shapes.\cr
optraProcrustes: Auxiliary optra subroutine of the Hartigan-Wong k-means for 3D shapes.\cr
outlierHipam: Individuals of the hipam clusters with 1 or 2 elements.\cr
overlappingRows: Overlapped biclusters by rows.\cr
parallelepiped8: Parallelepiped of 8 landmarks.\cr
parallelepiped34: Parallelepiped of 34 landmarks.\cr
plotMedoids: Medoids representation.\cr
plotTreeHipam: HIPAM dendogram.\cr
plotTrimmOutl: Trimmed or outlier observations representation.\cr
qtranProcrustes: Auxiliary qtran subroutine of the Hartigan-Wong k-means for 3D shapes.\cr
screeArchetyp: Screeplot of archetypes and archetypoids.\cr
shapes3dMod: 3D shapes plot.\cr
skeletonsArchet: Skeleton plots of archetypal individuals.\cr
stepArchetypesMod: Archetype algorithm to raw data.\cr
stepArchetypoids: Run the archetypoid algorithm several times.\cr
TDDclust: Trimmed clustering based on L1 data depth.\cr
trimmedLloydShapes: Trimmed Lloyd k-means for 3D shapes.\cr
trimmedoid: Trimmed k-medoids algorithm.\cr
trimowa: Trimmed PAM with OWA operators.\cr
WeightsMixtureUB: Calculation of the weights for the OWA operators.\cr
xyplotPCA: PC scores for archetypes.\cr
}
\author{
Guillermo Vinue <Guillermo.Vinue@uv.es>, Irene Epifanio, Amelia Simo, M. Victoria Ibanez, Juan Domingo, Guillermo Ayala
}
\keyword{ANTHROP}
