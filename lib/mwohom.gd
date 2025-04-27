############################################################################
##
#W  mwohom.gd             GAP4 package `groupoids'             Chris Wensley
#W                                                              & Emma Moore
##  
##  Declaration file for mappings of magmas with objects. 

############################################################################## 
## 
#C  IsGeneralMappingWithObjects( <map> )
#C  IsNonSPGeneralMappingWithObjects( <map> )
#C  IsSPGeneralMappingWithObjects( <map> )
## 
#?  do we need the SP and nonSP division ?
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
    IsMagmaWithObjectsGeneralMapping and IsSPGeneralMappingWithObjects );

#?  modify these next three to SynonymAttr's ??
DeclareProperty( "IsSemigroupWithObjectsHomomorphism", 
    IsMagmaWithObjectsHomomorphism  );
DeclareProperty( "IsMonoidWithObjectsHomomorphism", 
    IsMagmaWithObjectsHomomorphism );
DeclareProperty( "IsGroupWithObjectsHomomorphism", 
    IsMagmaWithObjectsHomomorphism );

InstallTrueMethod( IsMagmaWithObjectsHomomorphism, IsSemigroupWithObjectsHomomorphism );
InstallTrueMethod( IsMagmaWithObjectsHomomorphism, IsMonoidWithObjectsHomomorphism );
InstallTrueMethod( IsMagmaWithObjectsHomomorphism, IsGroupWithObjectsHomomorphism );

############################################################################## 
## 
#C  IsGroupoidHomomorphism( <map> )
## 
DeclareCategory( "IsGroupoidHomomorphism", IsMagmaWithObjectsHomomorphism ); 

#############################################################################
##
#C  IsGroupoidHomomorphismCollection . . . . category of colls of gpd homs
#C  IsGroupoidHomomorphismCollColl . . . . . category of colls of colls 
#C  IsGroupoidHomomorphismCollCollColl . . . category of colls, colls, colls
##
DeclareCategoryCollections( "IsGroupoidHomomorphism" );
DeclareCategoryCollections( "IsGroupoidHomomorphismCollection" );
DeclareCategoryCollections( "IsGroupoidHomomorphismCollColl" );

############################################################################## 
## 
#R  IsMappingToSinglePieceRep( <map> )                
#R  IsMappingWithPiecesRep( <map> )
#T  IsMWOMappingToSinglePieceType( <map> )
#T  IsGroupoidMappingToSinglePieceType( <map> )
#T  IsMWOMappingWithPiecesType( <map> ) 
#T  IsGrouppooidMappingWithPiecesType( <map> )
## 
##  A mapping to a single piece magma with objects is determined by:
##   - mappings from each magma in the source to the range magma,
##   - images for the objects.
##  A mapping whose range has more than one constituent 
##     requires a different representation.
## 
DeclareRepresentation( "IsMappingToSinglePieceRep",
    IsMagmaWithObjectsHomomorphism and IsAttributeStoringRep 
        and IsComponentObjectRep and IsGeneralMapping, 
    [ "Source", "Range", "MappingToSinglePieceData" ] ); 
DeclareRepresentation( "IsMappingWithPiecesRep", 
    IsMagmaWithObjectsHomomorphism and IsAttributeStoringRep 
        and IsComponentObjectRep and IsGeneralMapping, 
    [ "Source", "Range", "PiecesOfMapping" ] );
BindGlobal( "IsMWOMappingToSinglePieceType", 
            NewType( GeneralMappingWithObjectsFamily, 
            IsMappingToSinglePieceRep and IsMagmaWithObjectsHomomorphism ) );
BindGlobal( "IsGroupoidMappingToSinglePieceType", 
            NewType( GeneralMappingWithObjectsFamily, 
            IsMappingToSinglePieceRep and IsGroupoidHomomorphism ) );
BindGlobal( "IsMWOMappingWithPiecesType", 
            NewType( GeneralMappingWithObjectsFamily, 
            IsMappingWithPiecesRep and IsMagmaWithObjectsHomomorphism ) );
BindGlobal( "IsGroupoidMappingWithPiecesType", 
            NewType( GeneralMappingWithObjectsFamily, 
            IsMappingWithPiecesRep and IsGroupoidHomomorphism ) );

############################################################################## 
## 
#P  IsConstantOnObjects( <map> ) 
#P  IsGeneralMappingToSinglePiece( <map> ) 
#P  IsGeneralMappingFromSinglePiece( <map> ) 
#P  IsHomomorphismToSinglePiece( <map> ) 
#P  IsHomomorphismFromSinglePiece( <map> ) 
## 
##  A mapping to a connected magma is: 
##    - a list of mappings from the pieces of the source, 
##    - a list of lists of images of objects
## 
DeclareProperty("IsConstantOnObjects", IsGeneralMappingWithObjects); 
DeclareProperty("IsGeneralMappingToSinglePiece", IsGeneralMappingWithObjects);
DeclareProperty("IsGeneralMappingFromSinglePiece", IsGeneralMappingWithObjects);
DeclareProperty("IsMappingWithObjectsByFunction", IsGeneralMappingWithObjects);
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
#O  MappingWithObjectsByFunction( <src>, <rng>, <fun>, <imobs> )
#O  HomomorphismFromSinglePieceNC( <src>, <rng>, <hom>, <imobs> )  
#O  HomomorphismFromSinglePiece( <src>, <rng>, <hom>, <imobs> )  
#O  HomomorphismToSinglePieceNC( <src>, <rng>, <piece images> )  
#O  HomomorphismToSinglePiece( <src>, <rng>, <piece images> )  
#O  HomomorphismByUnionNC( <g1>, <g2>, <mors> )  
#O  HomomorphismByUnion( <g1>, <g2>, <list> )  
## 
DeclareOperation( "MappingWithObjectsByFunction", 
  [ IsMagmaWithObjects, IsMagmaWithObjects, IsFunction, IsHomogeneousList ] );
DeclareOperation( "HomomorphismFromSinglePieceNC",
  [ IsSinglePiece, IsSinglePiece, IsMagmaHomomorphism, IsHomogeneousList ] );
DeclareOperation( "HomomorphismFromSinglePiece",
  [ IsSinglePiece, IsSinglePiece, IsMagmaHomomorphism, IsHomogeneousList ] );
DeclareOperation( "HomomorphismToSinglePieceNC",
  [ IsMagmaWithObjects, IsSinglePiece, IsHomogeneousList ] );
DeclareOperation( "HomomorphismToSinglePiece",
  [ IsMagmaWithObjects, IsSinglePiece, IsHomogeneousList ] );
DeclareOperation( "HomomorphismByUnionNC",
    [ IsMagmaWithObjects, IsMagmaWithObjects, IsList ] );
DeclareOperation( "HomomorphismByUnion",
    [ IsMagmaWithObjects, IsMagmaWithObjects, IsList ] );

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
#A  MappingToSinglePieceData( <map> )
#A  MappingToSinglePieceMaps( <map> )
#A  PiecesOfMapping( <map> )                                    
#A  PartitionOfSource( <map> )
#A  ImagesOfObjects( <map> ) 
#A  ImageElementsOfRays( <map> ) 
#A  UnderlyingFunction( <map> )
## 
DeclareAttribute( "MappingToSinglePieceData", IsMappingToSinglePieceRep );
DeclareAttribute( "MappingToSinglePieceMaps", IsMappingToSinglePieceRep );
DeclareAttribute( "PiecesOfMapping", IsMappingWithPiecesRep );  
DeclareAttribute( "PartitionOfSource", IsMappingWithPiecesRep );
DeclareAttribute( "ImagesOfObjects", IsMagmaWithObjectsHomomorphism ); 
DeclareAttribute( "ImageElementsOfRays", IsMagmaWithObjectsHomomorphism ); 
DeclareAttribute( "UnderlyingFunction", IsMappingWithObjectsByFunction ); 

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
