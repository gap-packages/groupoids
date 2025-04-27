############################################################################
##
#W  util.gd                 GAP4 package `groupoids'           Chris Wensley
##  
##  Declaration file for utility functions in the groupoids package.

#############################################################################
##
#O  InclusionMappingGroups( <G>, <H> )
#O  MappingToOne( <G>, <H> );
#O  EndoMappingToOne( <G> );
##
DeclareOperation( "InclusionMappingGroups", [ IsGroup, IsGroup ] );
DeclareOperation( "MappingToOne", [ IsGroup, IsGroup ] );
DeclareOperation( "EndoMappingToOne", [ IsGroup ] );
