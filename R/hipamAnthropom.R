hipamAnthropom <- function(x,asw.tol=0,maxsplit=5,local.const=NULL,orness=0.7,type,ahVect=c(23,28,20,25,25),...){
 #Initialize the tree:
 tree <- initialize.tree(x, maxsplit, orness, type,...)
 #Local hipam:
 tree <- hipam.local(tree, x, asw.tol, maxsplit, local.const, orness, type, ...)
 tree
}


