##############################################################################
##
#W  mwohom.gd                GAP4 package `Gpd'                  Chris Wensley
#W                                                                & Emma Moore
#Y  Copyright (C) 2000-2016, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  Declaration file for functions in the Gpd package.
##

############################################################################## 
## 
#C  IsGeneralMappingWithObjects( <map> )
#C  IsNonSPGeneralMappingWithObjects( <map> )
#C  IsSPGeneralMappingWithObjects( <map> )
## 
#T  do we need the SP and nonSP division ?
##  Category and groupoid mapping declarations 

DeclareCategory( "IsGeneralMappingWithObjects", IsGeneralMapping ); 
DeclareCategory( "IsSPGeneralMappingWithObjects", 
    IsSPGeneralMapping and IsGeneralMappingWithObjects ); 
DeclareCategory( "IsNonSPGeneralMappingWithObjects", 
    IsNonSPGeneralMapping and IsGeneralMappingWithObjects ); 

#############################################################################
##
#C  IsGeneralMappingWithObjectsCollection . . category of colls of mwo maps
#C  IsGeneralMappingWithObjectsCollColl . . . category of colls of colls 
#C  IsGeneralMappingWithObjectsCollCollColl . calegory of colls, colls, colls
##
DeclareCategoryCollections( "IsGeneralMappingWithObjects" );
DeclareCategoryCollections( "IsGeneralMappingWithObjectsCollection" );
DeclareCategoryCollections( "IsGeneralMappingWithObjectsCollColl" );

############################################################################# 
##  
#V  GeneralMappingWithObjectsFamily .. family for homs of magmas with objects 
##  
BindGlobal( "GeneralMappingWithObjectsFamily", 
    NewFamily( "GeneralMappingWithObjectsFamily", 
               IsGeneralMappingWithObjects, 
               CanEasilySortElements, CanEasilySortElements ) ); 

#############################################################################
##
#P  IsMappingWithObjects( <map> )
#P  IsEndoGeneralMappingWithObjects( <map> )
#P  IsEndoMappingWithObjects( <map> )
#P  IsMagmaWithObjectsGeneralMapping( <map> ) 
#P  IsMagmaWithObjectsHomomorphism( <map> ) 
#P  IsSemigroupWithObjectsHomomorphism( <map> )
#P  IsMonoidWithObjectsHomomorphism( <map> )
#P  IsGroupWithObjectsHomomorphism( <map> )                
DeclareSynonymAttr( "IsMappingWithObjects",
    IsGeneralMappingWithObjects and IsMapping );
DeclareSynonymAttr( "IsEndoGeneralMappingWithObjects",
    IsGeneralMappingWithObjects and IsEndoGeneralMapping );
DeclareSynonymAttr( "IsEndoMappingWithObjects",
    IsMappingWithObjects and IsEndoMapping );
DeclareSynonymAttr( "IsMagmaWithObjectsGeneralMapping", 
    IsGeneralMappingWithObjects and RespectsMultiplication );
DeclareSynonymAttr( "IsMagmaWithObjectsHomomorphism", 
    IsMagmaWithObjectsGeneralMapping and IsMapping );

#?  modify these next three to SynonymAttr's ??
DeclareProperty( "IsSemigroupWithObjectsHomomorphism", 
    IsMagmaWithObjectsHomomorphism  );
DeclareProperty( "IsMonoidWithObjectsHomomorphism", 
    IsMagmaWithObjectsHomomorphism );
DeclareProperty( "IsGroupWithObjectsHomomorphism", 
    IsMagmaWithObjectsHomomorphism );

############################################################################## 
## 
#C  IsGroupoidHomomorphism( <map> )
## 
##  (29/04/10) : replaced the Property with a Category : 
##  DeclareProperty( "IsGroupoidHomomorphism", 
##      IsMagmaWithObjectsHomomorphism );
##  
DeclareCategory( "IsGroupoidHomomorphism", ## IsGeneralMappingWithObjects ); 
    IsMagmaWithObjectsHomomorphism ); 

#############################################################################
##
#C  IsGroupoidHomomorphismCollection . . . . category of colls of god ohms
#C  IsGroupoidHomomorphismCollColl . . . . . category of colls of colls 
#C  IsGroupoidHomomorphismCollCollColl . . . category of colls, colls, colls
##
DeclareCategoryCollections( "IsGroupoidHomomorphism" );
DeclareCategoryCollections( "IsGroupoidHomomorphismCollection" );
DeclareCategoryCollections( "IsGroupoidHomomorphismCollColl" );

############################################################################# 
##  
#V  GroupoidHomomorphismFamily . . . . . family for homomorphisms of groupies 
##  
BindGlobal( "GroupoidHomomorphismFamily", 
    NewFamily( "GroupoidHomomorphismFamily", IsGroupoidHomomorphism, 
               CanEasilySortElements, CanEasilySortElements ) ); 


############################################################################## 
## 
#R  IsMappingToSinglePieceRep( <map> )                
#R  IsMappingWithPiecesRep( <map> )
## 
##  A mapping to a single piece magma with objects is determined by:
##   - mappings from each magma in the source to the range magma,
##   - images for the objects.
##  A mapping whose range has more than one constituent 
##     requires a different representation.
## 
DeclareRepresentation( "IsMappingToSinglePieceRep",
    IsMagmaWithObjectsHomomorphism and IsAttributeStoringRep 
        and IsGeneralMapping,                              #? (07/10/08) 
    [ "Source", "Range", "PieceImages" ] ); 
DeclareRepresentation( "IsMappingWithPiecesRep", 
    IsMagmaWithObjectsHomomorphism and IsAttributeStoringRep 
        and IsGeneralMapping, 
    [ "Source", "Range", "PiecesOfMapping" ] );

############################################################################## 
## 
#P  IsConstantOnObjects( <map> ) 
#P  IsGeneralMappingToSinglePiece( <map> ) 
#P  IsGeneralMappingFromSinglePiece( <map> ) 
#P  IsHomomorphismToSinglePiece( <map> ) 
#P  IsHomomorphismFromSinglePiece( <map> ) 
## 
##  A mapping to a connected magma is: 
##    - a list of mappings from the constituents of the source, 
##    - a list of lists of images of objects
## 
DeclareProperty("IsConstantOnObjects", IsGeneralMappingWithObjects );  
DeclareProperty("IsGeneralMappingToSinglePiece", IsGeneralMappingWithObjects);
##                 IsGeneralMappingToSinglePiece );  
DeclareProperty("IsGeneralMappingFromSinglePiece", IsGeneralMappingWithObjects);
##                 IsGeneralMappingToSinglePiece );  
DeclareSynonymAttr( "IsHomomorphismToSinglePiece", 
    IsGeneralMappingToSinglePiece and IsMagmaWithObjectsHomomorphism );
DeclareSynonymAttr( "IsHomomorphismFromSinglePiece", 
    IsGeneralMappingFromSinglePiece and IsMagmaWithObjectsHomomorphism );

############################################################################# 
##  
##  MagmaWithObjectsHomomorphism( <args> )
## 
DeclareGlobalFunction( "MagmaWithObjectsHomomorphism" ); 

############################################################################# 
## 
#O  HomomorphismFromSinglePieceNC( <src>, <rng>, <hom>, <imobs> )  
#O  HomomorphismFromSinglePiece( <src>, <rng>, <hom>, <imobs> )  
#O  HomomorphismToSinglePieceNC( <src>, <rng>, <piece images> )  
#O  HomomorphismToSinglePiece( <src>, <rng>, <piece images> )  
#O  HomomorphismByUnionNC( <g1>, <g2>, <mors> )  
#O  HomomorphismByUnion( <g1>, <g2>, <list> )  
#O  HomomorphismFromSinglePieceGeneratorsImages( <src>, <rng>, <gens>, <ims> )
## 
DeclareOperation( "HomomorphismFromSinglePieceNC",
  [ IsSinglePiece, IsSinglePiece, IsMagmaHomomorphism, IsHomogeneousList ] );
DeclareOperation( "HomomorphismFromSinglePiece",
  [ IsSinglePiece, IsSinglePiece, IsMagmaHomomorphism, IsHomogeneousList ] );
DeclareOperation( "HomomorphismToSinglePieceNC",
  [ IsMagmaWithObjects, IsSinglePiece, IsHomogeneousList ] );
DeclareOperation( "HomomorphismToSinglePiece",
  [ IsMagmaWithObjects, IsSinglePiece, IsHomogeneousList ] );
DeclareOperation( "HomomorphismByUnionNC",
    [ IsMagmaWithObjects, IsMagmaWithObjects, IsHomogeneousList ] );
DeclareOperation( "HomomorphismByUnion",
    [ IsMagmaWithObjects, IsMagmaWithObjects, IsHomogeneousList ] );
DeclareOperation( "HomomorphismFromSinglePieceGeneratorsImages",
  [ IsSinglePiece, IsSinglePiece, IsHomogeneousList, IsHomogeneousList ] );

############################################################################# 
## 
#O  IsomorphismNewObjects( <mag>, <obs> )
##  
##  ?? (16/04/10) should be something like IsomorphismByObjectImages ?? 
## 
DeclareOperation( "IsomorphismNewObjects", 
    [ IsDomainWithObjects, IsHomogeneousList ] );

############################################################################# 
## 
#A  PieceImages( <map> )
#A  HomsOfMapping( <map> ) 
#A  PiecesOfMapping( <map> )                                    
#A  PartitionOfSource( <map> )
#A  ImagesOfObjects( <map> ) 
#A  ImagesOfRays( <map> ) 
## 
DeclareAttribute( "PieceImages", IsMappingToSinglePieceRep );
DeclareAttribute( "HomsOfMapping", IsHomomorphismToSinglePiece );  
DeclareAttribute( "PiecesOfMapping", IsMappingWithPiecesRep );  
DeclareAttribute( "PartitionOfSource", IsMappingWithPiecesRep );
DeclareAttribute( "ImagesOfObjects", IsMagmaWithObjectsHomomorphism ); 
DeclareAttribute( "ImagesOfRays", IsMagmaWithObjectsHomomorphism ); 

############################################################################# 
## 
#P  IsInjectiveOnObjects( <map> )                        
#P  IsSurjectiveOnObjects( <map> )                        
#P  IsBijectiveOnObjects( <map> ) 
#P  IsEndomorphismWithObjects( <map> ) 
#P  IsAutomorphismWithObjects( <map> ) 
##
DeclareProperty( "IsInjectiveOnObjects", IsMagmaWithObjectsHomomorphism );  
DeclareProperty( "IsSurjectiveOnObjects", IsMagmaWithObjectsHomomorphism );  
DeclareProperty( "IsBijectiveOnObjects", IsMagmaWithObjectsHomomorphism );  
DeclareProperty( "IsEndomorphismWithObjects", IsMagmaWithObjectsHomomorphism );
DeclareProperty( "IsAutomorphismWithObjects", IsMagmaWithObjectsHomomorphism );
  
############################################################################# 
## 
#A  InverseMapping( <map> )                                    
## 
DeclareAttribute( "InverseMapping",
    IsMagmaWithObjectsHomomorphism and IsInjectiveOnObjects 
                                   and IsSurjectiveOnObjects );  
  
############################################################################## 
## 
#E  mwohom.gd  . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here 
## 
