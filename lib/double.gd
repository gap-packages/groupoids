############################################################################# 
## 
#W  double.gd               GAP4 package `Groupoids'            Chris Wensley 
##
#Y  Copyright (C) 2000-2022, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  This file contains the declarations for douible DoubleGroupoids
##  

############################################################################# 
## 
#O  SquareOfArrows( <dmwo>, <elt>, <down>, <left>, <up>, <right> ) 
#O  MultiplicativeSquareWithObjects( 
##      <dmwo>, <elt>, <down>, <left>, <up>, <right> ) 
#O  SquareOfArrowsNC( <isdgpdelt>, <elt>, <down>, <left>, <up>, <right> ) 
## 
DeclareOperation( "SquareOfArrows", 
    [ IsDoubleGroupoid, IsMultiplicativeElement, 
      IsObject, IsObject, IsObject, IsObject ] ); 
DeclareSynonym( "MultiplicativeSquareWithObjects", SquareOfArrows ); 
DeclareOperation( "SquareOfArrowsNC", 
    [ IsBool, IsMultiplicativeElement, 
      IsObject, IsObject, IsObject, IsObject ] ); 
    
############################################################################## 
## 
#O  ElementOfSquare( <swo> ) 
#O  DownArrow( <swo> ) 
#O  LeftArrow( <swo> ) 
#O  UpArrow( <swo> ) 
#O  RightArrow( <swo> ) 
##  
DeclareOperation( "ElementOfSquare", [ IsDoubleGroupoidElement ] ); 
DeclareOperation( "DownArrow", [ IsDoubleGroupoidElement ] ); 
DeclareOperation( "LeftArrow", [ IsDoubleGroupoidElement ] ); 
DeclareOperation( "UpArrow", [ IsDoubleGroupoidElement ] ); 
DeclareOperation( "RightArrow", [ IsDoubleGroupoidElement ] ); 

############################################################################# 
## 
#O  UpDownProduct( <swo>, <elte>, <elt2> ) 
#O  LeftRightProduct( <swo>, <elte>, <elt2> ) 
## 
DeclareOperation( "UpDownProduct", 
    [ IsDoubleGroupoid, IsDoubleGroupoidElement, IsDoubleGroupoidElement ] ); 
DeclareOperation( "LeftRightProduct", 
    [ IsDoubleGroupoid, IsDoubleGroupoidElement, IsDoubleGroupoidElement ] ); 

############################################################################# 
##  
#O  SinglePieceDoubleGroupoid( <gpd>, <gp> ) 
#O  DoubleGroupoid( <gpd>, <gps> ) 
## 
DeclareOperation( "SinglePieceDoubleGroupoid", [ IsGroupoid, IsGroup ] ); 
DeclareOperation( "DoubleGroupoid", [ IsGroupoid, IsList ] ); 

