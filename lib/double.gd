############################################################################# 
## 
#W  double.gd               GAP4 package `Groupoids'            Chris Wensley 
##
##  
##  This file contains the declarations for basic double groupoids
##  

############################################################################# 
#C  IsDoubleGroupoid( <ddwo> ) . . . . . . . . . . . . . . . and all inverses
#C  IsBasicDoubleGroupoid( <ddwo>  . . . . . . . . . . . . . . . . no prexmod
#V  IsDoubleGroupoidFamily . . . . . . . . . . .  family for double groupoids 
#T  IsDoubleGroupoidType( <gpd> )
#T  IsDoubleGroupoidPiecesType( <dwo> )
#A  GroupoidOfDoubleGroupoid( <dwo> )
##
DeclareCategory( "IsDoubleGroupoid", 
    IsMonoidWithObjects and IsDoubleGroupoidElementCollection ); 
DeclareCategory( "IsBasicDoubleGroupoid", IsDoubleGroupoid ); 
DeclareCategoryCollections( "IsDoubleGroupoid" ); 
IsDoubleGroupoidFamily := CollectionsFamily( IsDoubleGroupoidElementFamily ); 
BindGlobal( "IsDoubleGroupoidType", 
            NewType( IsDoubleGroupoidFamily, 
                     IsMWOSinglePieceRep and IsDoubleGroupoid ) );
BindGlobal( "IsDoubleGroupoidPiecesType", 
            NewType( IsDoubleGroupoidFamily, 
                     IsPiecesRep and IsDoubleGroupoid and IsAssociative ) );
DeclareAttribute( "GroupoidOfDoubleGroupoid", IsDoubleGroupoid );

############################################################################# 
## 
#O  SquareOfArrows( <dmwo>, <elt>, <up>, <left>, <right>, <down> ) 
#O  MultiplicativeSquareWithObjects(  <dmwo>, <elt>, <up>, <lt>, <rt>, <dn> ) 
#O  SquareOfArrowsNC( <dmwo>, <elt>, <up>, <left>, <right>, <down> ) 
## 
DeclareOperation( "SquareOfArrows", 
    [ IsDoubleGroupoid, IsMultiplicativeElement, 
      IsObject, IsObject, IsObject, IsObject ] ); 
DeclareSynonym( "MultiplicativeSquareWithObjects", SquareOfArrows ); 
DeclareOperation( "SquareOfArrowsNC", 
    [ IsDoubleGroupoid, IsMultiplicativeElement,
      IsObject, IsObject, IsObject, IsObject ] ); 
    
############################################################################# 
## 
#O  ElementOfSquare( <swo> ) 
#O  DownArrow( <swo> ) 
#O  LeftArrow( <swo> ) 
#O  UpArrow( <swo> ) 
#O  RightArrow( <swo> ) 
#O  BoundaryOfSquare( <swo> )
#O  DoubleGroupoidOfSquare( <swo> )
##  
DeclareOperation( "ElementOfSquare", [ IsDoubleGroupoidElement ] ); 
DeclareOperation( "DownArrow", [ IsDoubleGroupoidElement ] ); 
DeclareOperation( "LeftArrow", [ IsDoubleGroupoidElement ] ); 
DeclareOperation( "UpArrow", [ IsDoubleGroupoidElement ] ); 
DeclareOperation( "RightArrow", [ IsDoubleGroupoidElement ] ); 
DeclareOperation( "BoundaryOfSquare", [ IsDoubleGroupoidElement ] ); 
DeclareOperation( "DoubleGroupoidOfSquare", [ IsDoubleGroupoidElement ] );

############################################################################# 
## 
#P  IsCommutingSquare( <sq> )
##
DeclareProperty( "IsCommutingSquare", IsDoubleGroupoidElement );

############################################################################# 
## 
#O  VerticalProduct(  <sq1>, <sq2> ) 
#O  HorizontalProduct( <sq1>, <sq2> )
#O  VerticalIdentities( <sq> )
#O  HorizontalIdentities( <sq> )
#O  VerticalInverse( <sq> )
#O  HorizontalInverse( <sq> )
## 
DeclareOperation( "VerticalProduct", 
    [ IsDoubleGroupoidElement, IsDoubleGroupoidElement ] ); 
DeclareOperation( "HorizontalProduct", 
    [ IsDoubleGroupoidElement, IsDoubleGroupoidElement ] );
DeclareOperation( "VerticalIdentities", [ IsDoubleGroupoidElement ] );
DeclareOperation( "HorizontalIdentities", [ IsDoubleGroupoidElement ] );
DeclareOperation( "VerticalInverse", [ IsDoubleGroupoidElement ] );
DeclareOperation( "HorizontalInverse", [ IsDoubleGroupoidElement ] );

############################################################################# 
## 
#O  TransposedSquare( <sq> )
#P  IsClosedUnderTransposition( <sq> )
##
DeclareOperation( "TransposedSquare",
    [ IsDoubleGroupoidElement ] );
DeclareProperty( "IsClosedUnderTransposition", IsDoubleGroupoidElement );

############################################################################# 
##  
#O  SinglePieceBasicDoubleGroupoid( <gpd> ) 
#F  DoubleGroupoid( <gpd> ) 
#O  DoubleGroupoidWithTrivialGroup( <obs> )
#O  DoubleGroupoidWithSingleObject( <gpd>, <ob> )
## 
DeclareOperation( "SinglePieceBasicDoubleGroupoid", [ IsGroupoid ] ); 
DeclareGlobalFunction( "DoubleGroupoid" ); 
DeclareOperation( "DoubleGroupoidWithTrivialGroup", [ IsList ] );
DeclareOperation( "DoubleGroupoidWithSingleObject", [ IsGroup, IsObject ] );

############################################################################# 
##  
#O  DoubleGroupoidHomomorphism( <src> <rng> <gpdhom> )
#C  IsDoubleGroupoidHomomorphism( <map> )
#C  IsDoubleGroupoidHomomorphismCollection . . category of colls of dgpd homs
#P  IsDoubleGroupoidEndomorphism( <mor> )
#A  UnderlyingGroupoidHomomorphism( <hom> )
## 
DeclareOperation( "DoubleGroupoidHomomorphism", 
    [ IsDoubleGroupoid, IsDoubleGroupoid, IsGroupoidHomomorphism ] ); 
DeclareCategory( "IsDoubleGroupoidHomomorphism",
    IsMagmaWithObjectsHomomorphism ); 
DeclareCategoryCollections( "IsDoubleGroupoidHomomorphism" );
DeclareSynonym( "IsDoubleGroupoidEndomorphism", 
    IsDoubleGroupoidHomomorphism and IsEndomorphismWithObjects );  
DeclareAttribute( "UnderlyingGroupoidHomomorphism",
    IsDoubleGroupoidHomomorphism );

############################################################################## 
## 
#R  IsDefaultDoubleGroupoidHomomorphismRep( <map> ) 
## 
##  A mapping of connected double groupoids is determined by
##  a groupoid homomorphism, and this defines:
##   - mapping from the root group in the source to that in the range,
##   - images for the objects, 
##   - images for the rays.
##  In this representation we essentially duplicate the gpd hom rep. 
##  
DeclareRepresentation( "IsDefaultDoubleGroupoidHomomorphismRep",
    IsDoubleGroupoidHomomorphism and IsAttributeStoringRep 
    and IsComponentObjectRep, [ "Source", "Range", "PiecesOfMapping",
                                "UnderlyingGroupoidHomomorphism" ] ); 

############################################################################# 
##  
#V  DoubleGroupoidHomomorphismFamily  . . family for homs of double groupoids 
#T  DoubleGroupoidHomomorphismType  . . . . type for homs of double groupoids 
##  
BindGlobal( "DoubleGroupoidHomomorphismFamily", 
    NewFamily( "DoubleGroupoidHomomorphismFamily",
               IsDoubleGroupoidHomomorphism, 
               CanEasilySortElements, CanEasilySortElements ) ); 
BindGlobal( "DoubleGroupoidHomomorphismType", 
    NewType( DoubleGroupoidHomomorphismFamily, 
             IsDefaultDoubleGroupoidHomomorphismRep
             and IsDoubleGroupoidHomomorphism ) );

############################################################################
##
#E double.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##  
