############################################################################
##
#W  gpdaut.gd             GAP4 package `groupoids'             Chris Wensley
#W                                                              & Emma Moore

############################################################################# 
## 
#P  IsGroupoidAutomorphism( <mor> )                        
#P  IsGroupoidAutomorphismByGroupAuto( <aut> )
#P  IsGroupoidAutomorphismByObjectPerm( <aut> )
#P  IsGroupoidAutomorphismByRayShifts( <aut> ) 
#P  IsGroupoidAutomorphismByPiecesPerm( <aut> )
##
DeclareSynonym( "IsGroupoidAutomorphism", 
    IsGroupoidHomomorphism and IsAutomorphismWithObjects );  
DeclareProperty( "IsGroupoidAutomorphismByGroupAuto", IsGroupoidAutomorphism );
DeclareProperty( "IsGroupoidAutomorphismByObjectPerm", IsGroupoidAutomorphism );
DeclareProperty( "IsGroupoidAutomorphismByPiecesPerm", IsGroupoidAutomorphism );
DeclareProperty( "IsGroupoidAutomorphismByRayShifts", IsGroupoidAutomorphism );

InstallTrueMethod(IsGroupoidAutomorphism, IsGroupoidAutomorphismByGroupAuto);
InstallTrueMethod(IsGroupoidAutomorphism, IsGroupoidAutomorphismByObjectPerm);
InstallTrueMethod(IsGroupoidAutomorphism, IsGroupoidAutomorphismByPiecesPerm);
InstallTrueMethod(IsGroupoidAutomorphism, IsGroupoidAutomorphismByRayShifts);
 
############################################################################# 
## 
#O  GroupoidAutomorphismByGroupAutoNC( <gpd>, <auto> )
#O  GroupoidAutomorphismByGroupAuto( <src>, <auto> )
## 
DeclareOperation( "GroupoidAutomorphismByGroupAutoNC", 
    [ IsGroupoid and IsSinglePiece, IsGroupHomomorphism ] );
DeclareOperation( "GroupoidAutomorphismByGroupAuto", 
    [ IsGroupoid and IsSinglePiece, IsGroupHomomorphism ] );

############################################################################# 
## 
#O  GroupoidAutomorphismByObjectPermNC( <gpd>, <oims> )
#O  GroupoidAutomorphismByObjectPerm( <gpd>, <oims> )
#O  GroupoidAutomorphismByNtupleNC( <gpd>, <L> ) 
#O  GroupoidAutomorphismByNtuple( <gpd>, <L> ) 
#O  GroupoidAutomorphismByRayShiftsNC( <gpd>, <rims> ) 
#O  GroupoidAutomorphismByRayShifts( <gpd>, <rims> ) 
## 
DeclareOperation( "GroupoidAutomorphismByObjectPermNC", 
    [ IsGroupoid, IsHomogeneousList ] );
DeclareOperation( "GroupoidAutomorphismByObjectPerm", 
    [ IsGroupoid, IsHomogeneousList ] );
DeclareOperation( "GroupoidAutomorphismByNtupleNC", 
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ] );
DeclareOperation( "GroupoidAutomorphismByNtuple", 
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ] ); 
DeclareOperation( "GroupoidAutomorphismByRayShiftsNC", 
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ] );
DeclareOperation( "GroupoidAutomorphismByRayShifts", 
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ] ); 

############################################################################# 
## 
#O  GroupoidInnerAutomorphism( <gpd>, <arr> )
#O  GroupoidInnerAutomorphismNormalSubgroupoid( <gpd>, <nsgpd>, <arr> )
##
DeclareOperation( "GroupoidInnerAutomorphism", 
    [ IsGroupoid, IsGroupoidElement ] ); 
DeclareOperation( "GroupoidInnerAutomorphismNormalSubgroupoid", 
    [ IsGroupoid, IsGroupoid, IsGroupoidElement ] ); 

############################################################################# 
## 
#P  IsAutomorphismGroupOfGroupoid( <gp> ) 
#P  IsGroupOfGroupoidAutomorphisms( <gp> ) 
#A  AutomorphismGroupOfGroupoid( <gpd> ) 
#O  NiceObjectAutoGroupGroupoid( <gpd>, <aut> )
#A  EmbeddingsInNiceObject( <gp> ) 
##  
DeclareProperty( "IsAutomorphismGroupOfGroupoid", IsGroup );
DeclareProperty( "IsGroupOfGroupoidAutomorphisms", IsGroup );
DeclareAttribute( "AutomorphismGroupOfGroupoid", IsGroupoid ); 
DeclareOperation( "NiceObjectAutoGroupGroupoid", [ IsGroupoid, IsGroup ] );
DeclareAttribute( "EmbeddingsInNiceObject", IsGroup ); 

InstallTrueMethod( IsGroup, IsAutomorphismGroupOfGroupoid );
InstallTrueMethod( IsGroup, IsGroupOfGroupoidAutomorphisms );

############################################################################# 
## 
#A  AutomorphismGroupoidOfGroupoid( <gpd> ) 
#A  IsomorphismClassesOfGroupoid( <gpd > ) 
##  
DeclareAttribute( "AutomorphismGroupoidOfGroupoid", IsGroupoid );
DeclareAttribute( "IsomorphismClassPositionsOfGroupoid", IsGroupoid );

## ======================================================================== ##
##                Homogeneous discrete groupoid automorphisms               ##
## ======================================================================== ##

############################################################################## 
## 
#P  IsAutomorphismOfHomogeneousDiscreteGroupoid( IsGroup ) 
#O  GroupoidAutomorphismByGroupAutosNC( <gpd, homs> )
#O  GroupoidAutomorphismByGroupAutos( <gpd, homs> )
#T  GroupoidHomomorphismDiscreteType  . . type for homomorphisms of groupoids 
##  
DeclareProperty( "IsAutomorphismOfHomogeneousDiscreteGroupoid", 
    IsGroupoidAutomorphism );
InstallTrueMethod(IsGroupoidAutomorphism, IsAutomorphismOfHomogeneousDiscreteGroupoid);

DeclareOperation( "GroupoidAutomorphismByGroupAutos", 
    [ IsHomogeneousDiscreteGroupoid, IsHomogeneousList ] );
DeclareOperation( "GroupoidAutomorphismByGroupAutosNC", 
    [ IsHomogeneousDiscreteGroupoid, IsHomogeneousList ] );
BindGlobal( "GroupoidHomomorphismDiscreteType", 
    NewType( GroupoidHomomorphismFamily, 
             IsGroupoidHomomorphismFromHomogeneousDiscreteRep 
             and IsGroupoidHomomorphismFromHomogeneousDiscrete ) );
    
## ======================================================================== ##
##                     Homogeneous groupoid automorphisms                   ##
## ======================================================================== ##

############################################################################# 
## 
#O  GroupoidAutomorphismByPiecesPermNC( <gpd>, <perm> )
#O  GroupoidAutomorphismByPiecesPerm( <gpd>, <perm> )
##  
DeclareOperation( "GroupoidAutomorphismByPiecesPermNC", 
    [ IsGroupoid, IsPerm ] );
DeclareOperation( "GroupoidAutomorphismByPiecesPerm", 
    [ IsGroupoid, IsPerm ] );

## ======================================================================== ##
##                              Groupoid actions.                           ##
## ======================================================================== ##

#############################################################################
##
#C  IsGroupoidAction( <map> )
#R  IsGroupoidActionRep( <map> )
#A  ActionMap( <act> )
##
##  A groupoid action is a mapping which satisfies the conjugation identities
##
DeclareCategory( "IsGroupoidAction", IsGeneralMapping );
DeclareRepresentation( "IsGroupoidActionRep", 
    IsGroupoidAction and IsAttributeStoringRep, 
    [ "Source", "Range", "ActionMap" ] );
DeclareAttribute( "ActionMap", IsGroupoidAction );

#############################################################################
##
#O  GroupoidActionByConjugation      groupoid G acts on itself by conjugation
##
DeclareOperation( "GroupoidActionByConjugation", [ IsGroupoid ] );

#############################################################################
##
#C  IsGroupoidActionCollection . . . . . . . category of colls of gpd actions
#C  IsGroupoidActionCollColl . . . . . . . . . . . category of colls of colls 
#C  IsGroupoidActionCollCollColl . . . . . .  category of colls, colls, colls
#V  GroupoidActionFamily . . . . . . . . . . . .  family for groupoid actions
#T  GroupoidActionType . . . . . . . . . . . . . .  type for groupoid actions 
##
DeclareCategoryCollections( "IsGroupoidAction" );
DeclareCategoryCollections( "IsGroupoidActionCollection" );
DeclareCategoryCollections( "IsGroupoidActionCollColl" );
BindGlobal( "GroupoidActionFamily", 
    NewFamily( "GroupoidActionFamily", IsGroupoidAction, 
               CanEasilySortElements, CanEasilySortElements ) ); 
BindGlobal( "GroupoidActionType", 
            NewType( GroupoidActionFamily, IsGroupoidActionRep ) ); 

