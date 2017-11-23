############################################################################# 
## 
#W  mwo.gd                 GAP4 package `groupoids'             Chris Wensley 
##
#Y  Copyright (C) 2000-2017, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  This file contains the declarations of elements, magma, etc., and their 
##  families in the case of many objects.  So each algebraic structure comes 
##  with a set of objects, and each element $e$ has a tail object $te$ and a 
##  head object $he$, and multiplies partially if composable: on the left 
##  with those elements in its family which have head $te$, and on the right 
##  with those elements of its family which have tail $he$.  Non-composable 
##  elements return $fail$ when multiplied. 
##  

###########################  DOMAIN WITH OBJECTS  ########################### 

############################################################################# 
## 
#C  IsDomainWithObjects( <obj> ) . . test if object is a domain with objects 
## 
DeclareCategory( "IsDomainWithObjects", IsDomain ); 

############################################################################ 
## 
#F  DomainWithObjects( <dom>, <obs> ) 
##
DeclareGlobalFunction( "DomainWithObjects" ); 

############################################################################# 
## 
#P  IsSinglePieceDomain( <dwo> ) . . . . . . . . . . . . . a connected domain
#P  IsDiscreteDomainWithObjects( <dwo> ) . . . . .  with at least two objects
#P  IsDirectProductWithCompleteDigraphDomain( <dwo> ) 
#P  IsHomogeneousDomainWithObjects( <dwo> ) 
## 
DeclareProperty( "IsSinglePieceDomain", IsDomainWithObjects );  
DeclareProperty( "IsDiscreteDomainWithObjects", IsDomainWithObjects ); 
DeclareProperty( "IsDirectProductWithCompleteDigraphDomain", 
    IsDomainWithObjects ); 
DeclareProperty( "IsHomogeneousDomainWithObjects", IsDomainWithObjects ); 

############################################################################# 
## 
#A  ObjectList( <dwo> ) 
#A  Pieces( <dwo> ) 
#A  RootObject( <dwo> )
## 
DeclareAttribute( "ObjectList", IsDomainWithObjects ); 
DeclareAttribute( "Pieces", IsDomainWithObjects );  
DeclareAttribute( "RootObject", IsSinglePieceDomain ); 
  
############################################################################## 
## 
#R  IsPiecesRep( <dwo> ) 
## 
##  A domain with objects is a list of single piece domains
## 
DeclareRepresentation( "IsPiecesRep", 
    IsComponentObjectRep and IsAttributeStoringRep and IsDomainWithObjects, 
    [ "Pieces", "ObjectList" ] ); 

############################################################################# 
## 
#O  UnionOfPieces( <pieces> )              
#O  UnionOfPiecesOp( <pieces>, <dom> )              
## 
DeclareGlobalFunction( "UnionOfPieces" );
DeclareOperation( "UnionOfPiecesOp", [ IsList, IsDomainWithObjects ] );    

############################################################################# 
## 
#O  PieceOfObject( <dwo>, <obj> )                                    
#O  PieceNrOfObject( <dwo>, <obj> )                                    
## 
DeclareOperation( "PieceOfObject", [ IsDomainWithObjects, IsObject ] );  
DeclareOperation( "PieceNrOfObject", [ IsDomainWithObjects, IsObject ] );

############################################################################# 
## 
#A  KindOfDomainWithObjects( <dwo> ) 
## 
DeclareAttribute( "KindOfDomainWithObjects", IsList );  


########################  MULT ELTS WITH OBJECTS  ########################### 

############################################################################# 
## 
#C  IsMultiplicativeElementWithObjects( <elt> ) 
## 
DeclareCategory( "IsMultiplicativeElementWithObjects", 
    IsMultiplicativeElement ); 
DeclareCategoryCollections( "IsMultiplicativeElementWithObjects" ); 

############################################################################# 
## 
#C  IsMultiplicativeElementWithObjectsAndOnes( <elt> ) 
## 
##  An identity element at object $o$ is an element $1_o$ which is a left 
##  identity for $e$ when $te=o$ and a right identity for $e$ when $he=o$. 
DeclareCategory( "IsMultiplicativeElementWithObjectsAndOnes", 
    IsMultiplicativeElementWithObjects ); 
DeclareCategoryCollections( "IsMultiplicativeElementWithObjectsAndOnes" ); 

############################################################################# 
## 
#C  IsMultiplicativeElementWithObjectsAndInverses( <elt> ) 
## 
##  An element $e$ has inverse $f$ provided $e*f=1_{te}$ and $f*e=1_{he}$. 
DeclareCategory( "IsMultiplicativeElementWithObjectsAndInverses", 
    IsMultiplicativeElementWithObjectsAndOnes ); 
DeclareCategoryCollections( 
    "IsMultiplicativeElementWithObjectsAndInverses" ); 

############################################################################# 
## 
#C  IsGroupoidElement( <elt> ) 
## 
DeclareCategory( "IsGroupoidElement", 
    IsMultiplicativeElementWithObjectsAndInverses ); 
DeclareCategoryCollections( "IsGroupoidElement" ); 

############################################################################# 
##  
#V  IsMultiplicativeElementWithObjectsFamily  . . family for elements of mwos 
#V  IsMultiplicativeElementWithObjectsAndOnesFamily . . . . . . and with ones
#V  IsMultiplicativeElementWithObjectsAndInversesFamily  . . . . and inverses
#V  IsGroupoidElementFamily  . . . . . . . . family for elements of groupoids
#T  IsMultiplicativeElementWithObjectsType  default type for elements of mwos 
#T  IsGroupoidElementType  . . . . . . default type for elements of groupoids
##  
BindGlobal( "IsMultiplicativeElementWithObjectsFamily", 
    NewFamily( "IsMultiplicativeElementWithObjectsFamily", 
               IsMultiplicativeElementWithObjects, 
               CanEasilySortElements, CanEasilySortElements ) ); 
BindGlobal( "IsMultiplicativeElementWithObjectsAndOnesFamily", 
    NewFamily( "IsMultiplicativeElementWithObjectsAndOnesFamily", 
               IsMultiplicativeElementWithObjectsAndOnes, 
               CanEasilySortElements, CanEasilySortElements ) ); 
BindGlobal( "IsMultiplicativeElementWithObjectsAndInversesFamily", 
    NewFamily( "IsMultiplicativeElementWithObjectsAndInversesFamily", 
               IsMultiplicativeElementWithObjectsAndInverses, 
               CanEasilySortElements, CanEasilySortElements ) ); 
BindGlobal( "IsGroupoidElementFamily", 
    NewFamily( "IsGroupoidElementFamily", IsGroupoidElement, 
               CanEasilySortElements, CanEasilySortElements ) ); 

BindGlobal( "IsMultiplicativeElementWithObjectsType", 
            NewType( IsMultiplicativeElementWithObjectsFamily, 
                     IsMultiplicativeElementWithObjects ) );
BindGlobal( "IsGroupoidElementType", 
            NewType( IsGroupoidElementFamily, IsGroupoidElement ) );

############################################################################## 
## 
#O  ElementOfArrow( <ewo> ) 
#O  TailOfArrow( <ewo> ) 
#O  HeadOfArrow( <ewo> ) 
##  
DeclareOperation( "ElementOfArrow", [ IsMultiplicativeElementWithObjects ] ); 
DeclareOperation( "TailOfArrow", [ IsMultiplicativeElementWithObjects ] ); 
DeclareOperation( "HeadOfArrow", [ IsMultiplicativeElementWithObjects ] ); 


###########################  MAGMA WITH OBJECTS  ############################ 

############################################################################# 
## 
#C  IsMagmaWithObjects( <dwo> ) . . . . . . . category of magmas with objects 
#C  IsMagmaWithObjectsAndOnes( <dwo> ) . . . . . . . . . . . . . . . and ones
#C  IsMagmaWithObjectsAndInverses( <dwo> ) . . . . . . . . . . .  and inverses
#C  IsGroupoid( <dwo> )  . . . . . . . . . . . . . . . . . . and all inverses
##
##  A *magma with objects* in {\GAP} is a domain $M$ with (not necessarily 
##  associative) partial mutliplication. 
## 
DeclareCategory( "IsMagmaWithObjects", IsDomainWithObjects and 
    IsMultiplicativeElementWithObjectsCollection );  
DeclareCategoryCollections( "IsMagmaWithObjects" ); 
DeclareCategory( "IsSemigroupWithObjects",
    IsMagmaWithObjects and IsAssociative ); 
DeclareCategory( "IsMonoidWithObjects", 
    IsSemigroupWithObjects and 
    IsMultiplicativeElementWithObjectsAndOnesCollection ); 
DeclareCategory( "IsGroupoid", IsMonoidWithObjects and 
    IsGroupoidElementCollection ); 
DeclareCategoryCollections( "IsGroupoid" ); 

############################################################################# 
##  
#V  IsMagmaWithObjectsFamily . . . . . . . . . family for magmas with objects 
#V  IsSemigroupWithObjectsFamily . . . . . family for semigroups with objects 
#V  IsMonoidWithObjectsFamily . . . . . . . . family for monoids with objects 
#V  IsGroupoidFamily . . . . . . . . . . . . . . . . . . family for groupoids
##  
IsMagmaWithObjectsFamily := CollectionsFamily( 
    IsMultiplicativeElementWithObjectsFamily ); 
IsSemigroupWithObjectsFamily := CollectionsFamily( 
    IsMultiplicativeElementWithObjectsAndOnesFamily ); 
IsMonoidWithObjectsFamily := CollectionsFamily( 
    IsMultiplicativeElementWithObjectsAndInversesFamily );  
IsGroupoidFamily := CollectionsFamily( IsGroupoidElementFamily ); 

############################################################################# 
## 
#P  IsSinglePiece( <mwo> )
#P  IsDiscrete( <mwo> ) 
#P  IsDirectProductWithCompleteDigraph( <mwo> )
## 
DeclareSynonymAttr( "IsSinglePiece", 
    IsMagmaWithObjects and IsSinglePieceDomain );
DeclareSynonymAttr( "IsDiscrete", 
    IsMagmaWithObjects and IsDiscreteDomainWithObjects );
DeclareSynonymAttr( "IsDirectProductWithCompleteDigraph", 
    IsMagmaWithObjects and IsDirectProductWithCompleteDigraphDomain );

############################################################################ 
## 
#F  MagmaWithObjects( <mag>, <obs> ) 
##
##  A standard single piece magma with objects has elements $(e,t,h)$ 
##  where $e$ is an element of <mag> and $t$, $h$ are the tail and head. 
##  Multiplication is given by  $(e,t,h)*(f,h,k) = (e*f,t,k)$. 
##  Other constructors are possible. 
## 
DeclareGlobalFunction( "MagmaWithObjects" ); 

############################################################################# 
## 
#A  GeneratorsOfMagmaWithObjects( <mwo> ) 
#A  GeneratorsOfSemigroupWithObjects( <mwo> ) 
#A  GeneratorsOfMonoidWithObjects( <mwo> ) 
## 
DeclareAttribute( "GeneratorsOfMagmaWithObjects", IsMagmaWithObjects ); 
DeclareAttribute( "GeneratorsOfSemigroupWithObjects", IsSemigroupWithObjects );
DeclareAttribute( "GeneratorsOfMonoidWithObjects", IsMonoidWithObjects ); 
    
############################################################################# 
## 
##  representation and types for groupoids etc with a single piece 
##
#R  IsMWOSinglePieceRep 
#T  IsMagmaWithObjectsType ( <mwo> )
#T  IsSemigroupWithObjectsType( <swo> )
#T  IsMonoidWithObjectsType( <mwo> )
#T  IsGroupoidType( <gpd> )
## 
DeclareRepresentation( "IsMWOSinglePieceRep", 
    IsComponentObjectRep and IsAttributeStoringRep and IsMagmaWithObjects, 
    [ "objects", "magma" ] ); 

BindGlobal( "IsMagmaWithObjectsType", 
            NewType( IsMagmaWithObjectsFamily, IsMWOSinglePieceRep ) );
BindGlobal( "IsSemigroupWithObjectsType", 
            NewType( IsSemigroupWithObjectsFamily, 
                     IsMWOSinglePieceRep and IsSemigroupWithObjects ) );
BindGlobal( "IsMonoidWithObjectsType", 
            NewType( IsMonoidWithObjectsFamily, 
                     IsMWOSinglePieceRep and IsMonoidWithObjects ) );
BindGlobal( "IsGroupoidType", 
            NewType( IsGroupoidFamily, 
                     IsMWOSinglePieceRep and IsGroupoid ) );

############################################################################# 
##  
##  types and properties for groupoids etc with several pieces
##
#T  IsMWOPiecesType( <dwo> )
#T  IsSemigroupWOPiecesType( <dwo> )
#T  IsMonoidWOPiecesType( <dwo> )
#T  IsGroupoidPiecesType( <dwo> )
#P  IsMagmaWithObjectsInPieces( <mwo> )
#P  IsSemigroupWithObjectsInPieces( <swo> )
#P  IsMonoidWithObjectsInPieces( <mwo> ) 
#P  IsGroupoidInPieces( <gpd> )
## 
BindGlobal( "IsMagmaWOPiecesType", 
            NewType( IsMagmaWithObjectsFamily, 
                     IsPiecesRep and IsMagmaWithObjects ) );
BindGlobal( "IsSemigroupWOPiecesType", 
            NewType( IsSemigroupWithObjectsFamily, 
                     IsPiecesRep and IsSemigroupWithObjects ) );
BindGlobal( "IsMonoidWOPiecesType", 
            NewType( IsMonoidWithObjectsFamily, 
                     IsPiecesRep and IsMonoidWithObjects ) );
BindGlobal( "IsGroupoidPiecesType", 
            NewType( IsGroupoidFamily, 
                     IsPiecesRep and IsGroupoid and IsAssociative ) );
#? maybe these will be useful one day? 
#? DeclareProperty( "IsMagmaWithObjectsInPieces", IsMagmaWithObjects ); 
#? DeclareProperty( "IsSemigroupWithObjectsInPieces", IsSemigroupWithObjects ); 
#? DeclareProperty( "IsMonoidWithObjectsInPieces", IsMonoidWithObjects ); 
#? DeclareProperty( "IsGroupoidInPieces", IsGroupoid ); 

############################################################################# 
## 
#O  SinglePieceMagmaWithObjects( <mag>, <obs> ) 
#O  DomainWithSingleObject( <mag>, <obj> )
## 
DeclareOperation( "SinglePieceMagmaWithObjects", [ IsMagma, IsCollection ] ); 
DeclareOperation( "DomainWithSingleObject", [ IsDomain, IsObject ] );    

############################################################################# 
## 
#O  Arrow( <mwo>, <elt>, <tail>, <head> ) 
#O  MultiplicativeElementWithObjects( <mwo>, <elt>, <tail>, <head> ) 
#O  ArrowNC( <isgpdelt>, <elt>, <tail>, <head> ) 
## 
DeclareOperation( "Arrow", 
    [ IsMagmaWithObjects, IsMultiplicativeElement, IsObject, IsObject ] ); 
DeclareSynonym( "MultiplicativeElementWithObjects", Arrow ); 
DeclareOperation( "ArrowNC", 
    [ IsBool, IsMultiplicativeElement, IsObject, IsObject ] ); 
    

################################  SEMIGROUPS  ###############################

############################################################################ 
## 
#F  SemigroupWithObjects( <mag>, <obs> )  
##
##  This is a magma with objects where the vertex magmas are semigroups. 
## 
DeclareGlobalFunction( "SemigroupWithObjects" ); 

############################################################################# 
## 
#O  SinglePieceSemigroupWithObjects( <sgp>, <obs> ) 
## 
DeclareOperation( "SinglePieceSemigroupWithObjects", 
    [ IsSemigroup, IsCollection ] ); 



#################################  MONOIDS  #################################

############################################################################ 
## 
#F  MonoidWithObjects( <mag>, <obs> )  
##
##  This is a magma with objects where the vertex magmas are monoids. 
## 
DeclareGlobalFunction( "MonoidWithObjects" ); 

############################################################################# 
## 
#O  SinglePieceMonoidWithObjects( <mon>, <obs> ) 
## 
DeclareOperation( "SinglePieceMonoidWithObjects", 
    [ IsMonoid, IsCollection ] ); 



#################################  GROUPS  ##################################

##  A *group with objects* is a magma with objects where 
##  the vertex magmas are groups, and every arrow has an inverse, 
##  and so is a *groupoid* - see file gpd.gd.




#################################  SUBDOMAINS  ############################## 

############################################################################# 
## 
#O  IsSubdomainWithObjects( <D>, <U> )
#F  SubdomainWithObjects( <args> )              
## 
DeclareOperation( "IsSubdomainWithObjects", 
    [ IsDomainWithObjects, IsDomainWithObjects ] );
DeclareGlobalFunction( "SubdomainWithObjects" );

############################################################################# 
## 
#O  SubdomainByPieces( <dwo>, <comp> )              
#O  PiecePositions( <dwo>, <sdwo> )
#O  DiscreteSubdomain( <dwo>, <obs>, <doms> )              
#A  MaximalDiscreteSubdomain( <dwo> )  
#O  FullSubdomain( <dwo>, <obs> )              
## 
DeclareOperation( "SubdomainByPieces",
    [ IsDomainWithObjects, IsHomogeneousList ] );
DeclareOperation( "PiecePositions", 
    [ IsDomainWithObjects, IsDomainWithObjects ] );
DeclareOperation( "DiscreteSubdomain",
    [ IsDomainWithObjects, IsHomogeneousList, IsHomogeneousList ] );
DeclareAttribute( "MaximalDiscreteSubdomain", IsDomainWithObjects );
DeclareOperation( "FullSubdomain", [IsDomainWithObjects,IsHomogeneousList] );


################################  SUBMAGMAS  ################################ 

############################################################################# 
## 
#R  IsSubmagmaWithObjectsTableRep 
##
DeclareRepresentation( "IsSubmagmaWithObjectsTableRep", 
    IsComponentObjectRep and IsAttributeStoringRep and IsMagmaWithObjects, 
    [ "objects", "magma", "table" ] ); 

############################################################################ 
## 
#O  IsSubmagmaWithObjectsGeneratingTable( <swo>, <A> )  
#O  SubmagmaWithObjectsElementsTable( <mag>, <A> )  
#O  SubmagmaWithObjectsByElementsTable( <swo>, <A> )  

DeclareOperation( "IsSubmagmaWithObjectsGeneratingTable", [IsMagma,IsList] );
DeclareOperation( "SubmagmaWithObjectsElementsTable", [IsMagma,IsList] );
DeclareOperation( "SubmagmaWithObjectsByElementsTable", 
    [ IsMagmaWithObjects, IsList ] );



################################ UTILITIES ################################### 

############################################################################# 
##                         
#O  Ancestor( <dwo> ) 
## 
DeclareOperation( "Ancestor", [ IsDomainWithObjects ] );    

#############################################################################
##
#E  mwo.gd  . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##  
