##############################################################################
##
#W  gpdhom.gd                GAP4 package `Gpd'                  Chris Wensley
#W                                                                & Emma Moore
#Y  Copyright (C) 2000-2016, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

############################################################################# 
##  
##  GroupoidHomomorphism( <args> )
## 
DeclareGlobalFunction( "GroupoidHomomorphism" ); 

############################################################################# 
## 
#A  IsomorphismPermGroupoid( <gpd> )
#A  IsomorphismSmallPermGroupoid( <gpd> )
## 
DeclareAttribute( "IsomorphismPermGroupoid", IsGroupoid );
DeclareAttribute( "IsomorphismSmallPermGroupoid", IsGroupoid );

############################################################################# 
## 
#P  IsGroupoidEndomorphism( <mor> )                        
#P  IsGroupoidAutomorphism( <mor> )                        
##
DeclareSynonym( "IsGroupoidEndomorphism", 
    IsGroupoidHomomorphism and IsEndomorphismWithObjects );  
DeclareSynonym( "IsGroupoidAutomorphism", 
    IsGroupoidHomomorphism and IsAutomorphismWithObjects );  
  
############################################################################# 
## 
#O  InclusionMappingGroupoids( <G>, <H> )
#O  RestrictedMappingGroupoids( <hom>, <src> )
#? #O  HomomorphismToTrivialGroupoid( <G>, <H> )
## 
DeclareOperation( "InclusionMappingGroupoids", [ IsGroupoid, IsGroupoid ] );
DeclareOperation( "RestrictedMappingGroupoids", 
    [ IsGroupoidHomomorphism, IsGroupoid ] );

############################################################################# 
## 
#O  GroupoidHomomorphismFromDiscreteToSinglePiece( <src>, <rng>, <mors> )
##  
DeclareOperation( "GroupoidHomomorphismFromDiscreteToSinglePiece", 
    [ IsGroupoid and IsDiscrete, IsGroupoid and IsSinglePiece, 
      IsHomogeneousList ] );

############################################################################# 
## 
#O  IsomorphismStandardGroupoid( <gpd>, <obs> )
##  
DeclareOperation( "IsomorphismStandardGroupoid", 
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ] );


####################### functions from the 08.05 paper ####################### 

############################################################################## 
## 
#R  IsDefaultGroupoidHomomorphismRep( <map> ) 
## 
##  A mapping of connected groupoids is determined by:
##   - mapping from the root group in the source to that in the range,
##   - images for the objects, 
##   - images for the rays.
##  In this representation, however, we just store the source; the range; 
##  and then put all the necessary detail into the list PieceImages. 
##  This third attribute can vary in contents, according to context. 
##  For general groupoids a PieceImage is a list [hom,oims,rims], 
##  giving a root group hom; list of object images; list of ray images. 
##  
DeclareRepresentation( "IsDefaultGroupoidHomomorphismRep",
    IsGroupoidHomomorphism and IsAttributeStoringRep, 
    [ "Source", "Range", "PieceImages" ] ); 

############################################################################# 
##  
#O  MappingPermObjectsImages( <obs>, <ims> ) 
#O  MappingTransObjectsImages( <obs>, <ims> ) 
#A  ObjectTransformationOfGroupoidHomomorphism( <hom> ) 
## 
DeclareOperation( "MappingPermObjectsImages", [ IsList, IsList ] ); 
DeclareOperation( "MappingTransObjectsImages", [ IsList, IsList ] ); 
DeclareAttribute( "ObjectTransformationOfGroupoidHomomorphism", 
    IsDefaultGroupoidHomomorphismRep );

############################################################################# 
## 
#A  RootGroupHomomorphism( <map> ) 
#O  ObjectGroupHomomorphism( <map>, <obj> ) 
#O  GroupoidHomomorphismFromSinglePieceNC( <src>, <rng>, <hom>, <oims>, <rims> )  
#O  GroupoidHomomorphismFromSinglePiece( <src>, <rng>, <hom>, <oims>, <rims> )  
#O  GroupoidAutomorphismByGroupAutoNC( <gpd, ims> )
#O  GroupoidAutomorphismByGroupAuto( <gpd, auto> )
#O  GroupoidAutomorphismByObjectPermNC( <gpd, oims> )
#O  GroupoidAutomorphismByObjectPerm( <gpd, oims> )
#O  GroupoidAutomorphismByRayShiftsNC( <gpd, rims> ) 
#O  GroupoidAutomorphismByRayShifts( <gpd, rims> ) 
## 
DeclareAttribute( "RootGroupHomomorphism", 
    IsGroupoidHomomorphism and IsHomomorphismToSinglePiece );  
DeclareOperation( "ObjectGroupHomomorphism", 
    [ IsGroupoidHomomorphism, IsObject ] );
DeclareOperation( "GroupoidHomomorphismFromSinglePieceNC", 
    [ IsGroupoid, IsGroupoid, IsGroupHomomorphism, 
      IsHomogeneousList, IsHomogeneousList ] );
DeclareOperation( "GroupoidHomomorphismFromSinglePiece", 
    [ IsGroupoid, IsGroupoid, IsGroupHomomorphism, 
      IsHomogeneousList, IsHomogeneousList ] );
DeclareOperation( "GroupoidAutomorphismByGroupAutoNC", 
    [ IsGroupoid and IsSinglePiece, IsGroupHomomorphism ] );
DeclareOperation( "GroupoidAutomorphismByGroupAuto", 
    [ IsGroupoid and IsSinglePiece, IsGroupHomomorphism ] );
DeclareOperation( "GroupoidAutomorphismByObjectPermNC", 
    [ IsGroupoid, IsHomogeneousList ] );
DeclareOperation( "GroupoidAutomorphismByObjectPerm", 
    [ IsGroupoid, IsHomogeneousList ] );
DeclareOperation( "GroupoidAutomorphismByRayShiftsNC", 
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ] );
DeclareOperation( "GroupoidAutomorphismByRayShifts", 
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ] );

############################################################################# 
## 
#P  IsAutomorphismGroupOfGroupoid( <gp> ) 
#O  AutomorphismGroupOfGroupoid( <gpd> ) 
#O  NiceObjectAutoGroupGroupoid( <gpd>, <aut> )
#A  EmbeddingsInNiceObject( <gp> ) 
##  
DeclareProperty( "IsAutomorphismGroupOfGroupoid", IsGroup );
DeclareOperation( "AutomorphismGroupOfGroupoid", [ IsGroupoid ] ); 
DeclareOperation( "NiceObjectAutoGroupGroupoid", [ IsGroupoid, IsGroup ] );
DeclareAttribute( "EmbeddingsInNiceObject", IsGroup ); 

############################################################################# 
## 
##  this should be a method for \in, but cannot make that work at present 
## 
#O  InAutomorphismGroupOfGroupoid( <a, aut> )
##  
DeclareOperation( "InAutomorphismGroupOfGroupoid", 
    [ IsGroupoidHomomorphism, IsAutomorphismGroupOfGroupoid ] );

############################################################################# 
## 
#O  TestAllProductsUnderGroupoidHomomorphism( <hom> )
##  
DeclareOperation( "TestAllProductsUnderGroupoidHomomorphism", 
    [ IsGroupoidHomomorphism ] );

## ======================================================================== ##
##                     Homogeneous groupoid homomorphisms                   ##
## ======================================================================== ##

############################################################################## 
## 
#R  IsGroupoidHomomorphismFromHomogeneousdDiscreteRep( <map> ) 
## 
##  A groupoid mapping from a homogeneous, discrete groupoid is determined by:
##   - homs from the object groups in the source to those in the range,
##   - images for the objects. 
##  
DeclareRepresentation( "IsGroupoidHomomorphismFromHomogeneousDiscreteRep",
    IsGroupoidHomomorphism and IsAttributeStoringRep, 
    [ "Source", "Range", "ImagesOfObjects", "ObjectHomomorphisms" ] ); 

############################################################################# 
## 
#O  GroupoidHomomorphismFromHomogeneousDiscrete( <src>,<rng>,<homs>,<oims> ) 
#O  GroupoidHomomorphismFromHomogeneousDiscreteNC( <src>,<rng>,<homs>,<oims> ) 
#A  ObjectHomomorphisms( <map> ) 
#P  IsGeneralMappingFromHomogeneousDiscrete( <map> ) 
#P  IsGroupoidHomomorphismFromHomogeneousDiscrete( <map> ) 
#P  IsAutomorphismOfHomogeneousDiscreteGroupoid( IsGroup ) 
#O  GroupoidAutomorphismByGroupAutosNC( <gpd, homs> )
#O  GroupoidAutomorphismByGroupAutos( <gpd, homs> )
##  
DeclareOperation( "GroupoidHomomorphismFromHomogeneousDiscrete", 
    [ IsHomogeneousDiscreteGroupoid, IsGroupoid, IsHomogeneousList, 
      IsHomogeneousList ] ); 
DeclareOperation( "GroupoidHomomorphismFromHomogeneousDiscreteNC", 
    [ IsHomogeneousDiscreteGroupoid, IsGroupoid, IsHomogeneousList, 
      IsHomogeneousList ] ); 
DeclareAttribute( "ObjectHomomorphisms", 
    IsGroupoidHomomorphism and IsHomomorphismToSinglePiece );  
DeclareProperty("IsGeneralMappingFromHomogeneousDiscrete", 
    IsGeneralMappingWithObjects );
DeclareProperty("IsGroupoidHomomorphismFromHomogeneousDiscrete", 
    IsGeneralMappingWithObjects );
DeclareProperty( "IsAutomorphismOfHomogeneousDiscreteGroupoid", 
    IsGroupoidAutomorphism );
DeclareOperation( "GroupoidAutomorphismByGroupAutos", 
    [ IsHomogeneousDiscreteGroupoid, IsHomogeneousList ] );
DeclareOperation( "GroupoidAutomorphismByGroupAutosNC", 
    [ IsHomogeneousDiscreteGroupoid, IsHomogeneousList ] );

############################################################################## 
## 
#E  gpdhom.gd  . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here 
## 
