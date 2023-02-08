############################################################################# 
## 
#W  double.gd               GAP4 package `Groupoids'            Chris Wensley 
##
#Y  Copyright (C) 2000-2023, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  This file contains the declarations for basic double groupoids
##  

############################################################################# 
#C  IsDoubleGroupoid( <ddwo> ) . . . . . . . . . . . . . . . and all inverses
#C  IsBasicDoubleGroupoid( <ddwo>  . . . . . . . . . . . . . . . . no prexmod
#V  IsDoubleGroupoidFamily . . . . . . . . . . .  family for double groupoids 
#T  IsDoubleGroupoidType( <gpd> )
#T  IsDoubleGroupoidPiecesType( <dwo> )
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

############################################################################# 
## 
#O  SquareOfArrows( <dmwo>, <elt>, <up>, <left>, <right>, <down> ) 
#O  MultiplicativeSquareWithObjects(  <dmwo>, <elt>, <up>, <left>, <right>, <down> ) 
#O  SquareOfArrowsNC( <elt>, <up>, <left>, <right>, <down> ) 
## 
DeclareOperation( "SquareOfArrows", 
    [ IsDoubleGroupoid, IsMultiplicativeElement, 
          IsObject, IsObject, IsObject, IsObject ] ); 
DeclareSynonym( "MultiplicativeSquareWithObjects", SquareOfArrows ); 
DeclareOperation( "SquareOfArrowsNC", 
    [ IsMultiplicativeElement,  IsObject, IsObject, IsObject, IsObject ] ); 
    
############################################################################# 
## 
#O  ElementOfSquare( <swo> ) 
#O  DownArrow( <swo> ) 
#O  LeftArrow( <swo> ) 
#O  UpArrow( <swo> ) 
#O  RightArrow( <swo> ) 
#A  BoundaryOfSquare( <swo> ) 
##  
DeclareOperation( "ElementOfSquare", [ IsDoubleGroupoidElement ] ); 
DeclareOperation( "DownArrow", [ IsDoubleGroupoidElement ] ); 
DeclareOperation( "LeftArrow", [ IsDoubleGroupoidElement ] ); 
DeclareOperation( "UpArrow", [ IsDoubleGroupoidElement ] ); 
DeclareOperation( "RightArrow", [ IsDoubleGroupoidElement ] ); 
DeclareAttribute( "BoundaryOfSquare", IsDoubleGroupoidElement ); 

############################################################################# 
## 
#O  UpDownProduct( <dgpd>, <sq1>, <sq2> ) 
#O  LeftRightProduct( <dgpd>, <sq1>, <sq2> ) 
## 
DeclareOperation( "UpDownProduct", 
    [ IsDoubleGroupoid, IsDoubleGroupoidElement, IsDoubleGroupoidElement ] ); 
DeclareOperation( "LeftRightProduct", 
    [ IsDoubleGroupoid, IsDoubleGroupoidElement, IsDoubleGroupoidElement ] ); 

############################################################################# 
##  
#O  SinglePieceBasicDoubleGroupoid( <gpd> ) 
#F  DoubleGroupoid( <gpd> ) 
## 
DeclareOperation( "SinglePieceBasicDoubleGroupoid", [ IsGroupoid ] ); 
DeclareGlobalFunction( "DoubleGroupoid" ); 

############################################################################
##
#E double.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##  
