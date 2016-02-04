############################################################################# 
## 
#W  gpd.gd                    GAP4 package `Gpd'                Chris Wensley 
#W                                                               & Emma Moore
##  version 1.41, 04/02/2016 
##
#Y  Copyright (C) 2000-2016, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

############################################################################# 
##  
#O  RootGroup( <gpd> ) 
#O  RayElementsOfGroupoid( <gpd> ) 
#O  RaysOfGroupoid( <gpd> ) 
#O  GeneratorsOfGroupoid( <gpd> )
#O  PiecesOfGroupoid( <gpd> ) 
## 
#?  (03/10/08)  changed these from Attributes to Operations 
#?              and call equivalent MagmaWithObjects attributes
##
DeclareOperation( "RootGroup", [ IsGroupoid and IsSinglePiece ] ); 
DeclareOperation( "RayElementsOfGroupoid", [ IsGroupoid ] ); 
DeclareOperation( "RaysOfGroupoid", [ IsGroupoid ] ); 
DeclareOperation( "GeneratorsOfGroupoid", [ IsGroupoid ] );
DeclareOperation( "PiecesOfGroupoid", [ IsGroupoid ] );  
  
############################################################################## 
## 
#R  IsSinglePieceRaysRep( <gpd> ) 
#A  LargerDirectProductGroupoid( <gpd> ) 
## 
##  A connected groupoid with variable object groups is a wide subgroupoid 
##  of an IsDirectProductWithCompleteGraph with the same objects, and is 
##  determined by a root group, and a set of conjugating elements 
## 
DeclareRepresentation( "IsSinglePieceRaysRep",
  IsGroupoid and IsAttributeStoringRep and IsComponentObjectRep, 
  [ "parent", "rootGroup", "rays" ] ); 
DeclareAttribute( "LargerDirectProductGroupoid", IsSinglePieceRaysRep ); 
 
############################################################################# 
## 
#P  IsPermGroupoid( <gpd> )                                    
#P  IsFpGroupoid( <gpd> )                                    
#P  IsPcGroupoid( <gpd> )                                    
#P  IsMatrixGroupoid( <gpd> )                                    
#P  IsFreeGroupoid( <gpd> )                                    
## 
DeclareProperty( "IsPermGroupoid", IsGroupoid );  
DeclareProperty( "IsFpGroupoid", IsGroupoid );  
DeclareProperty( "IsPcGroupoid", IsGroupoid );  
DeclareProperty( "IsMatrixGroupoid", IsGroupoid );  
DeclareProperty( "IsFreeGroupoid", IsGroupoid );  
  
############################################################################# 
## 
#F  Groupoid( <args> )              
## 
DeclareGlobalFunction( "Groupoid" );

############################################################################# 
##           
#O  SinglePieceGroupoidNC( <group>, <objects> )              
#O  SinglePieceGroupoid( <group>, <objects> )              
#O  SubgroupoidWithRaysNC( <parent>, <rootgp>, <conj> )
#O  SubgroupoidWithRays( <parent>, <rootgp>, <conj> )
#O  SinglePieceGroupoidByGenerators( <ancestor>, <gens> )
## 
DeclareOperation( "SinglePieceGroupoidNC", 
    [ IsGroup, IsHomogeneousList ] ); 
DeclareOperation( "SinglePieceGroupoid", 
    [ IsGroup, IsHomogeneousList ] ); 
DeclareOperation( "SubgroupoidWithRaysNC", 
    [ IsGroupoid, IsGroup, IsHomogeneousList ] );
DeclareOperation( "SubgroupoidWithRays", 
    [ IsGroupoid, IsGroup, IsHomogeneousList ] );
DeclareOperation( "SinglePieceGroupoidByGenerators", 
    [ IsGroupoid, IsList ] ); 

############################################################################# 
##                         
#O  ObjectGroup( <gpd>, <obj> )
#A  ObjectGroups( <gpd> )
## 
DeclareOperation( "ObjectGroup", [ IsGroupoid, IsObject ] );    
DeclareAttribute( "ObjectGroups", IsGroupoid );    


## ======================================================================== ##
##                           Homogeneous groupoids                          ##
## ======================================================================== ##

############################################################################## 
## 
#P  IsHomogeneousDiscreteGroupoid( <gpd> ) 
#R  IsHomogeneousDiscreteGroupoidRep( <gpd> ) 
## 
##  A homogeneous, discrete groupoid is a union of identical, single domain 
##  groupoids - just the thing for the source of an xmod of groupoids! 
##  So such structures deserve their own representation. 
## 
DeclareSynonymAttr( "IsHomogeneousDiscreteGroupoid", IsGroupoid and 
    IsDiscreteDomainWithObjects and IsHomogeneousDomainWithObjects ); 
DeclareRepresentation( "IsHomogeneousDiscreteGroupoidRep",
  IsGroupoid and IsAttributeStoringRep and IsComponentObjectRep, 
  [ "magma", "objects" ] ); 

############################################################################## 
## 
#O  HomogeneousGroupoid( <gpd>, <obs> ) 
#O  HomogeneousGroupoidNC( <gpd>, <obs> ) 
#O  HomogeneousDiscreteGroupoid( <gp>, <obs> ) 
## 
DeclareOperation( "HomogeneousGroupoid", [ IsGroupoid, IsHomogeneousList ] ); 
DeclareOperation( "HomogeneousGroupoidNC", [ IsGroupoid, IsHomogeneousList ] ); 
DeclareOperation( "HomogeneousDiscreteGroupoid", 
    [ IsGroup, IsHomogeneousList ] ); 

## ======================================================================== ##
##                       Manipulating groupoid unions                       ##
## ======================================================================== ##

############################################################################## 
## 
#O  ReplaceOnePieceInUnion( <union>, <old>, <new> ) 
## 
DeclareOperation( "ReplaceOnePieceInUnion", 
    [ IsGroupoid and IsPiecesRep, IsObject,  IsGroupoid and IsSinglePiece ] ); 


## ======================================================================== ##
##                            Groupoid Elements                             ##
## ======================================================================== ##

############################################################################## 
## 
#O  GroupoidElement( <gpd>, <elt>, <src>, <tgt> ) 
## 
##  A connected groupoid element is [ group element, tail, head ]
## 
DeclareSynonym( "GroupoidElement", Arrow ); 

############################################################################## 
## 
##  Stars, Costars and Homsets are particular subsets of the elements
##  of a connected groupoid for which an iterator is required
##  
##  Six items of data are required: 
##  1.  the family of objects of the parent groupoid, 
##  2.  a set of group elements, e.g. an object group, 
##  3.  a list of source objects, 
##  4.  a list of target objects, 
##  5.  a list of rays, 
##  6.  a single character string "c" with c in {s,c,h,u,r,l,d}, 
##      specifying star; costar; homset; union; right/left/double coset.
##
#P  IsHomsetCosets( <obj> )
#R  IsHomsetCosetsRep( <obj> )
#O  ObjectStarNC( <gpd>, <obj> ) 
#O  ObjectStar( <gpd>, <obj> ) 
#O  ObjectCostarNC( <gpd>, <obj> ) 
#O  ObjectCostar( <gpd>, <obj> ) 
#O  HomsetNC( <gpd>, <obj1>, <obj2> ) 
#O  Homset( <gpd>, <obj1>, <obj2> ) 
#A  ElementsOfGroupoid( <gpd> ) 
##
DeclareProperty( "IsHomsetCosets", IsGroupoidElementCollection );
DeclareRepresentation( "IsHomsetCosetsRep", 
    IsHomsetCosets and IsPositionalObjectRep, [1..6] ); 
## DeclareAttribute( "UnionHomsets", IsGroupoid );
DeclareOperation( "IdentityArrow", [ IsGroupoid, IsObject ] ); 
DeclareOperation( "ObjectStarNC", [ IsGroupoid, IsObject ] ); 
DeclareOperation( "ObjectStar", [ IsGroupoid, IsObject ] ); 
DeclareOperation( "ObjectCostarNC", [ IsGroupoid, IsObject ] ); 
DeclareOperation( "ObjectCostar", [ IsGroupoid, IsObject ] ); 
DeclareOperation( "HomsetNC", [ IsGroupoid, IsObject, IsObject ] ); 
DeclareOperation( "Homset", [ IsGroupoid, IsObject, IsObject ] ); 
DeclareAttribute( "ElementsOfGroupoid", IsGroupoid ); 

############################################################################## 
##  
#?  (26/09/08)  RightCosetReps should be RightTransversal ?? 
##
#P  IsGroupoidCoset( <cset> ) 
#A  SuperDomain( <cset> );                        #? rename ?? 
#A  HomsetCosetsGroupoidCoset( <cset> ) 
#O  RightCosetRepresentatives( <gpd>, <sgpd> )    #? should be Iterator ?? 
#R  IsLeftCosetWithObjectsDefaultRep( <gp>, <obj> ) 
#O  LeftCosetRepresentatives( <gpd>, <sgpd> )     #? should be Iterator ?? 
#O  LeftCosetRepresentativesFromObject( <gpd>, <sgpd>, <obj> ) 
#O  LeftCosetsNC( <gpd>, <sgpd> ) 
#O  LeftCoset( <gpd>, <sgpd>, <elt> )
#O  DoubleCosetRepresentatives( <gpd>, <sgpd>, <sgpd> )        #? ditto ?? 
## 
DeclareProperty( "IsGroupoidCoset", IsRightCosetDefaultRep ); 
DeclareAttribute( "SuperDomain", IsRightCosetDefaultRep ); 
DeclareAttribute( "HomsetCosetsGroupoidCoset", IsRightCosetDefaultRep ); 
DeclareCategory( "IsLeftCosetWithObjects", IsDomain and IsExternalOrbit );
DeclareRepresentation( "IsLeftCosetWithObjectsDefaultRep", 
    IsComponentObjectRep and IsAttributeStoringRep 
    and IsLeftCosetWithObjects, [] );
DeclareOperation( "RightCosetRepresentatives", [ IsGroupoid, IsGroupoid ] ); 
DeclareOperation( "LeftCosetRepresentatives", [ IsGroupoid, IsGroupoid ] ); 
DeclareOperation( "LeftCosetRepresentativesFromObject", 
    [ IsGroupoid, IsGroupoid, IsObject ] ); 
DeclareOperation( "LeftCosetsNC", [ IsGroupoid, IsGroupoid ] ); 
DeclareOperation( "LeftCoset", [ IsGroupoid, IsGroupoid, IsGroupoidElement ] ); 
DeclareOperation( "DoubleCosetRepresentatives", 
    [ IsGroupoid, IsGroupoid, IsGroupoid ] ); 

############################################################################## 
## 
#O  IsElementInGroupoid( <e>, <gpd> ) 
##
##  this takes the place of an implementation of \in 
##  
DeclareOperation( "IsElementInGroupoid", [ IsGroupoidElement, IsGroupoid ] ); 


## ======================================================================== ##
##                               Subgroupoids                               ##
## ======================================================================== ##

############################################################################# 
## 
#F  Subgroupoid( <args> )              
#O  IsSubgroupoid( <G>, <U> )
## 
DeclareGlobalFunction( "Subgroupoid" );
DeclareOperation( "IsSubgroupoid", [ IsGroupoid, IsGroupoid ] );

############################################################################# 
## 
#O  SubgroupoidBySubgroup( <gpd>, <sgp> ) 
#O  SubgroupoidByPieces( <gpd>, <obhoms> )              
#O  PiecePositions( <gpd>, <sgpd> )
#O  DiscreteSubgroupoid( <gpd>, <gps>, <obs> )              
#A  MaximalDiscreteSubgroupoid( <gpd> )  
#O  FullSubgroupoid( <gpd>, <obs> )              
#A  FullTrivialSubgroupoid( <gpd> )  
#A  DiscreteTrivialSubgroupoid( <gpd> )  
## 
DeclareOperation( "SubgroupoidBySubgroup", 
    [ IsGroupoid and IsDirectProductWithCompleteGraph, IsGroup ] ); 
DeclareOperation( "SubgroupoidByPieces",
    [ IsGroupoid, IsList ] );
##  DeclareOperation( "PiecePositions", [ IsGroupoid, IsGroupoid ] );
DeclareOperation( "DiscreteSubgroupoid",
    [ IsGroupoid, IsList, IsHomogeneousList ] );
DeclareAttribute( "MaximalDiscreteSubgroupoid", IsGroupoid );
DeclareOperation( "FullSubgroupoid", [ IsGroupoid, IsHomogeneousList ] );
DeclareAttribute( "FullTrivialSubgroupoid", IsGroupoid );
DeclareAttribute( "DiscreteTrivialSubgroupoid", IsGroupoid );

#############################################################################
##
#O  ConjugateGroupoid( <gpd>, <elt> ) . . . . . . conjugate of <gpd> by <elt>
##
DeclareOperation( "ConjugateGroupoid", [ IsGroupoid, IsGroupoidElement ] );

#############################################################################
##
#O  ConjugateArrow( <x>, <y> ) . . . . groupoid conjugate of x by y
## 
#?  can now change this to  e^g  ??
DeclareOperation( "ConjugateArrow", [ IsGroupoidElement, IsGroupoidElement ] );

############################################################################## 
## 
#E  gpd.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here 
## 
