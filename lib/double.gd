############################################################################# 
## 
#W  double.gd               GAP4 package `Groupoids'            Chris Wensley 
##
#Y  Copyright (C) 2000-2022, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  This file contains the declarations for douible DoubleGroupoids
##  

####################  DOUBLE DOMAIN WITH OBJECTS  ########################### 

############################################################################# 
## 
#C  IsDoubleDomainWithObjects( <obj> ) . . is a double domain with objects? 
## 
DeclareCategory( "IsDoubleDomainWithObjects", IsDomainWithObjects ); 

############################################################################ 
## 
#F  DoubleDomainWithObjects( <dom>, <obs> ) 
##
DeclareGlobalFunction( "DoubleDomainWithObjects" ); 

############################################################################# 
## 
#P  IsSinglePieceDoubleDomain( <dwo> ) . . . . . . . a connected DoubleDomain
#P  IsDiscreteDoubleDomainWithObjects( <dwo> ) . .  with at least two objects
#P  IsDirectProductWithCompleteDigraphDoubleDomain( <dwo> ) 
#P  IsHomogeneousDoubleDomainWithObjects( <dwo> ) 
## 
DeclareProperty( "IsSinglePieceDoubleDomain", IsDoubleDomainWithObjects );  
DeclareProperty( "IsDiscreteDoubleDomainWithObjects", 
    IsDoubleDomainWithObjects ); 
DeclareProperty( "IsDirectProductWithCompleteDigraphDoubleDomain", 
    IsDoubleDomainWithObjects ); 
DeclareProperty( "IsHomogeneousDoubleDomainWithObjects", 
    IsDoubleDomainWithObjects ); 


########################  MULT SQUARE WITH OBJECTS  ######################### 

############################################################################# 
## 
#C  IsMultiplicativeSquareWithObjects( <elt> ) 
## 
DeclareCategory( "IsMultiplicativeSquareWithObjects", 
    IsMultiplicativeElement ); 
DeclareCategoryCollections( "IsMultiplicativeSquareWithObjects" ); 

############################################################################# 
## 
#C  IsMultiplicativeSquareWithObjectsAndOnes( <elt> ) 
## 
##  An identity element at object $o$ is an element $1_o$ which is a left 
##  identity for $e$ when $te=o$ and a right identity for $e$ when $he=o$. 
DeclareCategory( "IsMultiplicativeSquareWithObjectsAndOnes", 
    IsMultiplicativeSquareWithObjects ); 
DeclareCategoryCollections( "IsMultiplicativeSquareWithObjectsAndOnes" ); 

############################################################################# 
## 
#C  IsMultiplicativeSquareWithObjectsAndInverses( <elt> ) 
## 
##  An element $e$ has inverse $f$ provided $e*f=1_{te}$ and $f*e=1_{he}$. 
DeclareCategory( "IsMultiplicativeSquareWithObjectsAndInverses", 
    IsMultiplicativeSquareWithObjectsAndOnes ); 
DeclareCategoryCollections( 
    "IsMultiplicativeSquareWithObjectsAndInverses" ); 

############################################################################# 
## 
#C  IsDoubleGroupoidElement( <elt> ) 
#V  IsDoubleGroupoidElementFamily  . . family for elements of double groupoids
#T  IsDoubleGroupoidElementType . default type for elements of double groupoids
## 
DeclareCategory( "IsDoubleGroupoidElement", 
    IsMultiplicativeSquareWithObjectsAndInverses ); 
DeclareCategoryCollections( "IsDoubleGroupoidElement" ); 
DeclareCategory( "IsDoubleGroupoidByIsomorphismsElement", 
    IsMultiplicativeSquareWithObjectsAndInverses ); 
DeclareCategoryCollections( "IsDoubleGroupoidByIsomorphismsElement" ); 
BindGlobal( "IsDoubleGroupoidElementFamily", 
    NewFamily( "IsDoubleGroupoidElementFamily", IsDoubleGroupoidElement, 
               CanEasilySortElements, CanEasilySortElements ) ); 
BindGlobal( "IsDoubleGroupoidElementType", 
            NewType( IsDoubleGroupoidElementFamily, IsDoubleGroupoidElement ) );

############################################################################## 
## 
#O  ElementOfSquare( <ewo> ) 
#O  DownArrow( <ewo> ) 
#O  LeftArrow( <ewo> ) 
#O  UpArrow( <ewo> ) 
#O  RightArrow( <ewo> ) 
##  
DeclareOperation( "ElementOfSquare", [ IsMultiplicativeSquareWithObjects ] ); 
DeclareOperation( "DownArrow", [ IsMultiplicativeSquareWithObjects ] ); 
DeclareOperation( "LeftArrow", [ IsMultiplicativeSquareWithObjects ] ); 
DeclareOperation( "UpArrow", [ IsMultiplicativeSquareWithObjects ] ); 
DeclareOperation( "RightArrow", [ IsMultiplicativeSquareWithObjects ] ); 


#################### DOUBLE MAGMA WITH OBJECTS  ############################ 

############################################################################# 
## 
#C  IsDoubleMagmaWithObjects( <dwo> ) . category of double magmas with objects 
#C  IsDoubleMagmaWithObjectsAndOnes( <dwo> ) . . . . . . . . . . . . and ones
#C  IsDoubleMagmaWithObjectsAndInverses( <dwo> ) . . . . . . . . and inverses
#C  IsDoubleGroupoid( <dwo> )  . . . . . . . . . . . . . . . and all inverses
##
##  A *magma with objects* in {\GAP} is a DoubleDomain $M$ with  
##  (not necessarily associative) partial mutliplication. 
## 
DeclareCategory( "IsDoubleMagmaWithObjects", IsDoubleDomainWithObjects and 
    IsMultiplicativeSquareWithObjectsCollection );  
DeclareCategory( "IsDoubleGroupoid", IsDoubleMagmaWithObjects and 
    IsDoubleGroupoidElementCollection ); 
DeclareCategoryCollections( "IsDoubleGroupoid" ); 

############################################################################# 
##  
#O  SinglePieceDoubleGroupoid( <gpd>, <gp> ) 
#O  DoubleGroupoid( <gpd>, <gps> ) 
## 
DeclareOperation( "SinglePieceDoubleGroupoid", [ IsGroupoid, IsGroup ] ); 
DeclareOperation( "DoubleGroupoid", [ IsGroupoid, IsList ] ); 

############################################################################# 
##  
#V  IsDoubleGroupoidFamily . . . . . . . family and type for double groupoids 
#T  IsDoubleGroupoidType 
##  
IsDoubleGroupoidFamily := CollectionsFamily( IsDoubleGroupoidElementFamily ); 
BindGlobal( "IsDoubleGroupoidType", 
            NewType( IsDoubleGroupoidFamily, IsDoubleGroupoid ) ); 
    
############################################################################# 
## 
#O  MultiplicativeSquareWithObjects( 
##      <dmwo>, <elt>, <down>, <left>, <up>, <right> ) 
#O  MultiplicativeSquareWithObjectsNC( 
##      <isgpdelt>, <elt>, <down>, <left>, <up>, <right> ) 
## 
DeclareOperation( "MultiplicativeSquareWithObjects", 
    [ IsDoubleMagmaWithObjects, IsMultiplicativeElement, 
      IsObject, IsObject, IsObject, IsObject ] ); 
DeclareOperation( "MultiplicativeSquareWithObjectsNC", 
    [ IsBool, IsMultiplicativeElement, 
      IsObject, IsObject, IsObject, IsObject ] ); 
   
############################################################################# 
## 
#O  UpDownProduct( <dmwo>, <elte>, <elt2> ) 
#O  LeftRightProduct( <dmwo>, <elte>, <elt2> ) 
## 
DeclareOperation( "UpDownProduct", 
    [ IsDoubleMagmaWithObjects, IsMultiplicativeSquareWithObjects, 
      IsMultiplicativeSquareWithObjects ] ); 
DeclareOperation( "LeftRightProduct", 
    [ IsDoubleMagmaWithObjects, IsMultiplicativeSquareWithObjects, 
      IsMultiplicativeSquareWithObjects ] ); 
