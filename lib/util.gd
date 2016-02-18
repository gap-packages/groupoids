##############################################################################
##
#W  util.gd                    GAP4 package `Gpd'                Chris Wensley
##
#Y  Copyright (C) 2000-2016, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  Declaration file for utility functions in the Gpd package.
##

#############################################################################
##
#O  InclusionMappingGroups( <G>, <H> )
#O  MappingToOne( <G>, <H> );
#O  EndoMappingToOne( <G> );
##
DeclareOperation( "InclusionMappingGroups", [ IsGroup, IsGroup ] );
DeclareOperation( "MappingToOne", [ IsGroup, IsGroup ] );
DeclareOperation( "EndoMappingToOne", [ IsGroup ] );

#############################################################################
##
#E  util.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##  