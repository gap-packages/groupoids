##############################################################################
##
#W  gpdaut.gd             GAP4 package `groupoids'               Chris Wensley
#W                                                                & Emma Moore
#Y  Copyright (C) 2000-2018, Emma Moore and Chris Wensley,  
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
#O  AutomorphismGroupOfGroupoid( <gpd> ) 
#O  NiceObjectAutoGroupGroupoid( <gpd>, <aut> )
#A  EmbeddingsInNiceObject( <gp> ) 
#P  IsAutomorphismGroupOfGroupoidAsGroupoid( <gpd> ) 
##  
DeclareProperty( "IsAutomorphismGroupOfGroupoid", IsGroup );
DeclareOperation( "AutomorphismGroupOfGroupoid", [ IsGroupoid ] ); 
DeclareOperation( "NiceObjectAutoGroupGroupoid", [ IsGroupoid, IsGroup ] );
DeclareAttribute( "EmbeddingsInNiceObject", IsGroup ); 
DeclareProperty( "IsAutomorphismGroupOfGroupoidAsGroupoid", IsGroupoid );

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
#A  AutomorphismGroupoidOfGroupooid( <gpd> ) 
#O  GroupoidAutomorphismByPiecesPermNC( <gpd>, <perm> )
#O  GroupoidAutomorphismByPiecesPerm( <gpd>, <perm> )
##  
DeclareAttribute( "AutomorphismGroupoidOfGroupoid", IsGroupoid );
DeclareOperation( "GroupoidAutomorphismByPiecesPermNC", 
    [ IsGroupoid, IsPerm ] );
DeclareOperation( "GroupoidAutomorphismByPiecesPerm", 
    [ IsGroupoid, IsPerm ] );
    
##############################################################################
##
#E  gpdaut.gd  . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##
