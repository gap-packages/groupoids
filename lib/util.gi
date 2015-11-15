#############################################################################
##
#W  util.gi                    GAP4 package `Gpd'               Chris Wensley
##
##  version 1.36, 04/11/2015 
##
#Y  Copyright (C) 2000-2015, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  Installation file for utility functions in Gpd and XMod packages.
## 

#############################################################################
##
#V  InfoGpd
##
DeclareInfoClass( "InfoGpd" );
SetInfoLevel( InfoGpd, 1 );

#############################################################################
##
#M  InclusionMappingGroups( <G>, <H> )
##
InstallMethod( InclusionMappingGroups, "generic method for subgroup",
               IsIdenticalObj, [ IsGroup, IsGroup ], 0,
function( G, H )

    local  genH, inc, ok;
    if not IsSubgroup( G, H ) then
        Error( "usage: InclusionMappingGroups( G, H );  with  H <= G" );
    fi; 
    genH := GeneratorsOfGroup( H ); 
    inc := GroupHomomorphismByImages( H, G, genH, genH ); 
    if ( genH = [ ] ) then 
        SetIsInjective( inc, true ); 
    else 
        ok := IsInjective( inc ); 
    fi; 
    return inc;
end );

#############################################################################
##
#M  MappingToOne( <G>, <H> )
##
InstallMethod( MappingToOne, "generic method for groups",
    true, [ IsGroup, IsGroup ], 0,
function( G, H )

    local  genG, oneH, ones;
    genG := GeneratorsOfGroup( G );
    oneH := One( H );
    ones := List( genG, g -> oneH );
    return GroupHomomorphismByImages( G, H, genG, ones );
end );

#############################################################################
##
#M  EndoMappingToOne( <G> )
##
InstallMethod( EndoMappingToOne, "generic method for groups",
    true, [ IsGroup ], 0,
function( G )

    local  genG, oneG, ones;
    genG := GeneratorsOfGroup( G );
    oneG := One( G );
    ones := List( genG, g -> oneG );
    return GroupHomomorphismByImages( G, G, genG, ones );
end );

#############################################################################
##
#E  util.gi . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##  