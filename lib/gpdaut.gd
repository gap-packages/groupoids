##############################################################################
##
#W  gpdaut.gd             GAP4 package `groupoids'               Chris Wensley
#W                                                                & Emma Moore
#Y  Copyright (C) 2000-2019, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

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
#O  GroupoidAutomorphismByRayShiftsNC( <gpd>, <rims> ) 
#O  GroupoidAutomorphismByRayShifts( <gpd>, <rims> ) 
#O  GroupoidInnerAutomorphism( <gpd>, <arr> )
## 
DeclareOperation( "GroupoidAutomorphismByObjectPermNC", 
    [ IsGroupoid, IsHomogeneousList ] );
DeclareOperation( "GroupoidAutomorphismByObjectPerm", 
    [ IsGroupoid, IsHomogeneousList ] );
DeclareOperation( "GroupoidAutomorphismByRayShiftsNC", 
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ] );
DeclareOperation( "GroupoidAutomorphismByRayShifts", 
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ] ); 
DeclareOperation( "GroupoidInnerAutomorphism", 
    [ IsGroupoid, IsGroupoidElement ] ); 

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
