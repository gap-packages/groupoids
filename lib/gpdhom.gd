############################################################################
##
#W  gpdhom.gd             GAP4 package `groupoids'             Chris Wensley
#W                                                              & Emma Moore

############################################################################# 
##  
##  GroupoidHomomorphism( <args> )
## 
DeclareGlobalFunction( "GroupoidHomomorphism" ); 

############################################################################# 
## 
#A  IsomorphismPermGroupoid( <gpd> )
#A  IsomorphismPcGroupoid( <gpd> )
#A  RegularActionHomomorphismGroupoid( <gpd> )
## 
DeclareAttribute( "IsomorphismPermGroupoid", IsGroupoid );
DeclareAttribute( "IsomorphismPcGroupoid", IsGroupoid );
DeclareAttribute( "RegularActionHomomorphismGroupoid", IsGroupoid );

############################################################################# 
## 
#P  IsGroupoidEndomorphism( <mor> )                        
DeclareSynonym( "IsGroupoidEndomorphism", 
    IsGroupoidHomomorphism and IsEndomorphismWithObjects );  
 
############################################################################# 
## 
#O  InclusionMappingGroupoids( <G>, <H> )
#O  RestrictedMappingGroupoids( <hom>, <src> )
#A  ParentMappingGroupoids( <hom> )
## 
DeclareOperation( "InclusionMappingGroupoids", [ IsGroupoid, IsGroupoid ] );
DeclareOperation( "RestrictedMappingGroupoids", 
    [ IsGeneralMappingWithObjects, IsGroupoid ] );
DeclareAttribute( "ParentMappingGroupoids", IsGeneralMappingWithObjects ); 

############################################################################# 
## 
#O  IsomorphismStandardGroupoid( <gpd>, <obs> )
##  
DeclareOperation( "IsomorphismStandardGroupoid", 
    [ IsGroupoid, IsHomogeneousList ] );

############################################################################# 
## 
#O  IsomorphismGroupoids( <gpd1>, <gpd2> )
##  
DeclareOperation( "IsomorphismGroupoids", [ IsGroupoid, IsGroupoid ] );


####################### functions from the 08.05 paper ####################### 

############################################################################## 
## 
#R  IsDefaultGroupoidHomomorphismRep( <map> ) 
## 
##  A mapping of connected groupoids is determined by:
##   - mapping from the root group in the source to that in the range,
##   - images for the objects, 
##   - images for the rays.
##  In this representation, however, we just store the source; the range; and 
##  then put all the necessary detail into the list MappingToSinglePieceData. 
##  This third attribute can vary in contents, according to context. 
##  For general groupoids a PieceImage is a list [hom,oims,rims], 
##  giving a root group hom; list of object images; list of ray images. 
##  
DeclareRepresentation( "IsDefaultGroupoidHomomorphismRep",
    IsGroupoidHomomorphism and IsAttributeStoringRep and IsComponentObjectRep, 
    [ "Source", "Range", "PiecesOfMapping" ] ); 

############################################################################# 
##  
#V  GroupoidHomomorphismFamily  . . . . family for homomorphisms of groupoids 
#T  GroupoidHomomorphismType  . . . . . . type for homomorphisms of groupoids 
##  
BindGlobal( "GroupoidHomomorphismFamily", 
    NewFamily( "GroupoidHomomorphismFamily", IsGroupoidHomomorphism, 
               CanEasilySortElements, CanEasilySortElements ) ); 
BindGlobal( "GroupoidHomomorphismType", 
    NewType( GroupoidHomomorphismFamily, 
             IsDefaultGroupoidHomomorphismRep and IsGroupoidHomomorphism ) );

############################################################################# 
##  
#O  MappingPermObjectsImages( <obs>, <ims> ) 
#O  MappingTransObjectsImages( <obs>, <ims> ) 
#A  ObjectTransformationOfGroupoidHomomorphism( <hom> ) 
## 
DeclareOperation( "MappingPermObjectsImages", [ IsList, IsList ] ); 
DeclareOperation( "MappingTransObjectsImages", [ IsList, IsList ] ); 
DeclareAttribute( "ObjectTransformationOfGroupoidHomomorphism", 
    IsGroupWithObjectsHomomorphism );

############################################################################# 
## 
#A  RootGroupHomomorphism( <map> ) 
#O  ObjectGroupHomomorphism( <map>, <obj> ) 
#O  GroupoidHomomorphismFromSinglePieceNC( <src>, <rng>, <gens>, <images> )  
#O  GroupoidHomomorphismFromSinglePiece( <src>, <rng>, <gens>, <images> ) 
## 
DeclareAttribute( "RootGroupHomomorphism", 
    IsGroupoidHomomorphism and IsHomomorphismToSinglePiece );  
DeclareOperation( "ObjectGroupHomomorphism", 
    [ IsGroupoidHomomorphism, IsObject ] );
DeclareOperation( "GroupoidHomomorphismFromSinglePieceNC", 
    [ IsGroupoid, IsGroupoid, IsHomogeneousList, IsHomogeneousList ] );
DeclareOperation( "GroupoidHomomorphismFromSinglePiece", 
    [ IsGroupoid, IsGroupoid, IsHomogeneousList, IsHomogeneousList ] ); 

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
#R  IsGroupoidHomomorphismFromHomogeneousDiscreteRep( <map> ) 
## 
##  A groupoid mapping from a homogeneous, discrete groupoid is determined by:
##   - homs from the object groups in the source to those in the range,
##   - images for the objects. 
##  
DeclareRepresentation( "IsGroupoidHomomorphismFromHomogeneousDiscreteRep",
    IsGroupoidHomomorphism and IsAttributeStoringRep and IsComponentObjectRep, 
    [ "Source", "Range", "ImagesOfObjects", "ObjectHomomorphisms" ] ); 

############################################################################# 
## 
#O  GroupoidHomomorphismFromHomogeneousDiscrete( <src>,<rng>,<homs>,<oims> ) 
#O  GroupoidHomomorphismFromHomogeneousDiscreteNC( <src>,<rng>,<homs>,<oims> ) 
#A  ObjectHomomorphisms( <map> ) 
#P  IsGeneralMappingFromHomogeneousDiscrete( <map> ) 
#P  IsGroupoidHomomorphismFromHomogeneousDiscrete( <map> ) 
#P  IsGroupoidHomomorphismWithGroupoidByIsomorphisms( <map> ) 
##  
DeclareOperation( "GroupoidHomomorphismFromHomogeneousDiscrete", 
    [ IsHomogeneousDiscreteGroupoid, IsGroupoid, IsHomogeneousList, 
      IsHomogeneousList ] ); 
DeclareOperation( "GroupoidHomomorphismFromHomogeneousDiscreteNC", 
    [ IsHomogeneousDiscreteGroupoid, IsGroupoid, IsHomogeneousList, 
      IsHomogeneousList ] ); 
DeclareAttribute( "ObjectHomomorphisms", 
    IsGroupoidHomomorphism and IsHomomorphismToSinglePiece );  
DeclareProperty( "IsGeneralMappingFromHomogeneousDiscrete", 
    IsGeneralMappingWithObjects );
DeclareProperty( "IsGroupoidHomomorphismFromHomogeneousDiscrete", 
    IsGeneralMappingWithObjects );
DeclareProperty( "IsGroupoidHomomorphismWithGroupoidByIsomorphisms", 
    IsGeneralMappingWithObjects );
