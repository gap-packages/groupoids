############################################################################# 
## 
#W  mwo.gd                    GAP4 package `Gpd'                Chris Wensley 
##
##  version 1.33, 02/06/2015 
##
#Y  Copyright (C) 2000-2015, Emma Moore and Chris Wensley,  
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
#P  IsDirectProductWithCompleteGraphDomain( <dwo> ) 
#P  IsHomogeneousDomainWithObjects( <dwo> ) 
## 
DeclareProperty( "IsSinglePieceDomain", IsDomainWithObjects );  
DeclareProperty( "IsDiscreteDomainWithObjects", IsDomainWithObjects ); 
DeclareProperty( "IsDirectProductWithCompleteGraphDomain", 
    IsDomainWithObjects ); 
DeclareProperty( "IsHomogeneousDomainWithObjects", IsDomainWithObjects ); 

############################################################################# 
## 
#A  ObjectList( <dwo> ) 
#A  Pieces( <dwo> ) 
#O  RootObject( <dwo> )
## 
DeclareAttribute( "ObjectList", IsDomainWithObjects ); 
DeclareAttribute( "Pieces", IsDomainWithObjects );  
DeclareOperation( "RootObject", [ IsSinglePieceDomain ] ); 
  
############################################################################## 
## 
#R  IsPiecesRep( <dwo> )                
## 
##  A domain with objects is a list of single piece domains
## 
DeclareRepresentation( "IsPiecesRep", 
    IsDomainWithObjects and IsAttributeStoringRep, 
    [ "Pieces", "ObjectList" ] ); 
 
############################################################################# 
## 
#O  UnionOfPiecesNC( <pieces>, <type> )              
#O  UnionOfPieces( <pieces> )              
## 
DeclareOperation( "UnionOfPiecesNC", [ IsList, IsInt ] );    
DeclareOperation( "UnionOfPieces", [ IsList ] );

############################################################################# 
## 
#O  PieceOfObject( <dwo>, <obj> )                                    
#O  PieceNrOfObject( <dwo>, <obj> )                                    
## 
DeclareOperation( "PieceOfObject", [ IsDomainWithObjects, IsScalar ] );  
DeclareOperation( "PieceNrOfObject", [ IsDomainWithObjects, IsScalar ] );

############################################################################# 
## 
#O  TypeOfDomainWithObjects( <dwo> ) 
## 
DeclareOperation( "TypeOfDomainWithObjects", [ IsList ] );  


########################  MULT ELTS WITH OBJECTS  ########################### 

############################################################################# 
## 
#C  IsMultiplicativeElementWithObjects( <elt> ) 
## 
DeclareCategory( "IsMultiplicativeElementWithObjects", 
    IsMultiplicativeElement ); 
DeclareCategoryCollections( "IsMultiplicativeElementWithObjects" ); 
DeclareCategoryCollections( "IsMultiplicativeElementWithObjectsCollection" ); 
DeclareCategoryCollections( "IsMultiplicativeElementWithObjectsCollColl" ); 
##  DeclareSynonyms ??? 

############################################################################# 
## 
#C  IsMultiplicativeElementWithObjectsAndOnes( <elt> ) 
## 
##  An identity element at object $o$ is an element $1_o$ which is a left 
##  identity for $e$ when $te=o$ and a right identity for $e$ when $he=o$. 
DeclareCategory( "IsMultiplicativeElementWithObjectsAndOnes", 
    IsMultiplicativeElementWithObjects ); 
DeclareCategoryCollections( "IsMultiplicativeElementWithObjectsAndOnes" ); 
DeclareCategoryCollections( 
    "IsMultiplicativeElementWithObjectsAndOnesCollection" ); 
DeclareCategoryCollections( 
    "IsMultiplicativeElementWithObjectsAndOnesCollColl" ); 

############################################################################# 
## 
#C  IsMultiplicativeElementWithObjectsAndInverses( <elt> ) 
## 
##  An element $e$ has inverse $f$ provided $e*f=1_{te}$ and $f*e=1_{he}$. 
DeclareCategory( "IsMultiplicativeElementWithObjectsAndInverses", 
    IsMultiplicativeElementWithObjectsAndOnes ); 
DeclareCategoryCollections( 
    "IsMultiplicativeElementWithObjectsAndInverses" ); 
DeclareCategoryCollections( 
    "IsMultiplicativeElementWithObjectsAndInversesCollection" ); 
DeclareCategoryCollections(     
    "IsMultiplicativeElementWithObjectsAndInversesCollColl" ); 

############################################################################# 
## 
#C  IsGroupoidElement( <elt> ) 
## 
DeclareCategory( "IsGroupoidElement", 
    IsMultiplicativeElementWithObjectsAndInverses ); 
DeclareCategoryCollections( "IsGroupoidElement" ); 
DeclareCategoryCollections( "IsGroupoidElementCollection" ); 
DeclareCategoryCollections( "IsGroupoidElementCollColl" ); 

############################################################################# 
## 
#R  IsMultiplicativeElementWithObjectsPosRep 
## 
##  DeclareRepresentation( "IsMultiplicativeElementWithObjectsPosRep", 
##    IsPositionalObjectRep and IsMultiplicativeElementWithObjects, [1..3] ); 

############################################################################# 
##  
#V  MultiplicativeElementWithObjectsFamily  . . . family for elements of mwos 
#V  GroupoidElementFamily  . . . . . . . . . family for elements of groupoids
##  
BindGlobal( "MultiplicativeElementWithObjectsFamily", 
    NewFamily( "MultiplicativeElementWithObjectsFamily", 
               IsMultiplicativeElementWithObjects, 
               CanEasilySortElements, CanEasilySortElements ) ); 
BindGlobal( "GroupoidElementFamily", 
    NewFamily( "GroupoidElementFamily", IsGroupoidElement, 
               CanEasilySortElements, CanEasilySortElements ) ); 

############################################################################## 
## 
#A  ElementOfArrow( <ewo> ) 
#A  TailOfArrow( <ewo> ) 
#A  HeadOfArrow( <ewo> ) 
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
    IsMagma and IsMultiplicativeElementWithObjectsCollection ); 
DeclareCategoryCollections( "IsMagmaWithObjects" ); 
##  ?? (23/04/10) Declare more CategoryColections ?? 
DeclareCategory( "IsMagmaWithObjectsAndOnes", 
    IsMagmaWithObjects and 
    IsMultiplicativeElementWithObjectsAndOnesCollection ); 
DeclareCategory( "IsMagmaWithObjectsAndInverses", 
    IsMagmaWithObjectsAndOnes and 
    IsMultiplicativeElementWithObjectsAndInversesCollection ); 
##  DeclareCategory( "IsMagmaWithObjectsAndInverses",
DeclareCategory( "IsGroupoid", IsMagmaWithObjectsAndInverses 
    and IsGroupoidElementCollection ); 

############################################################################# 
##  
#V  MagmaWithObjectsFamily . . . . . . . . . . family for magmas with objects 
#V  GroupoidFamily . . . . . . . . . . . . . . . . . . . family for groupoids
##  
BindGlobal( "MagmaWithObjectsFamily", 
    NewFamily( "MagmaWithObjectsFamily", IsMagmaWithObjects, 
               CanEasilySortElements, CanEasilySortElements ) ); 
BindGlobal( "GroupoidFamily", NewFamily( "GroupoidFamily", IsGroupoid, 
               CanEasilySortElements, CanEasilySortElements ) ); 

############################################################################# 
## 
#P  IsSinglePiece( <mwo> )
#P  IsDiscrete( <mwo> ) 
#P  IsDirectProductWithCompleteGraph( <mwo> )
## 
DeclareSynonymAttr( "IsSinglePiece", 
    IsMagmaWithObjects and IsSinglePieceDomain );
DeclareSynonymAttr( "IsDiscrete", 
    IsMagmaWithObjects and IsDiscreteDomainWithObjects );
DeclareSynonymAttr( "IsDirectProductWithCompleteGraph", 
    IsMagmaWithObjects and IsDirectProductWithCompleteGraphDomain );

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
#O  GeneratorsOfMagmaWithObjects( <mwo> ) 
## 
DeclareOperation( "GeneratorsOfMagmaWithObjects", [ IsMagmaWithObjects ] ); 

############################################################################# 
## 
#R  IsMWOSinglePieceRep 
## 
#?  (30/04/10)  .eltsfam  deleted
##
DeclareRepresentation( "IsMWOSinglePieceRep", 
    IsComponentObjectRep and IsAttributeStoringRep and IsMagmaWithObjects, 
    [ "objects", "magma" ] ); 
    #? (07/09/11) add "rays" here ?? 

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
    [ IsMagmaWithObjects, IsMultiplicativeElement, IsScalar, IsScalar ] ); 
DeclareSynonym( "MultiplicativeElementWithObjects", Arrow ); 
DeclareOperation( "ArrowNC", 
    [ IsBool, IsMultiplicativeElement, IsScalar, IsScalar ] ); 
    
############################################################################## 
## 
#O  IsArrowIn( <e>, <mwo> ) 
##
##  this takes the place of an implementation of \in 
##  
DeclareOperation( "IsArrowIn", 
    [ IsMultiplicativeElementWithObjects, IsMagmaWithObjects ] ); 


################################  SEMIGROUPS  ###############################

#############################################################################
##
#P  IsSemigroupWithObjects( <mwo> )
##
##  returns `true' if the object <obj> is a semigroup & a magma with objects.
##
#?  why SynonymAttr ??  surely a category ?? 
##
DeclareSynonymAttr( "IsSemigroupWithObjects",
    IsMagmaWithObjects and IsAssociative );

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

#############################################################################
##
#P  IsMonoidWithObjects( <mwo> )
##
##  returns `true' if the object <obj> is a monoid and a magma with objects.
##
#?  why SynonymAttr -- see above ??
##
DeclareSynonymAttr( "IsMonoidWithObjects", 
    IsMagmaWithObjectsAndOnes and IsAssociative );

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

############################################################################## 
## 
#P  IsWide( <dwo>, <swo> )                            
## 
DeclareOperation( "IsWide", [ IsDomainWithObjects, IsDomainWithObjects ] ); 


################################  SUBMAGMAS  ################################ 

############################################################################# 
## 
#R  IsSubmagmaWithObjectsTableRep 
## 
#?  (30/04/10)  .eltsfam  removed 
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
