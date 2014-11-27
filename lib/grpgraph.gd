##############################################################################
##
#W  grpgraph.gd                GAP4 package `Gpd'                Chris Wensley
#W                                                                & Emma Moore
##  version 1.31, 09/11/2014 
##
#Y  Copyright (C) 2000-2014, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  This file contains the declarations for involutory FpWeightedDigraphs 
##  and FpWeightedDigraphs of groups, 
##  and normal forms for FpWeightedDigraph of groups. 
## 

############################################################################## 
## 
#C  IsDigraph( <dig> )                
## 
DeclareCategory( "IsDigraph", IsDomain );

############################################################################# 
## 
#O  Vertices( <dig> );  
#O  Arcs( <dig> );  
## 
##  Vertices must be declared as Operation, and not Attribute, 
##  so as not to conflict with Vertices in the Grape package. 
##   
DeclareOperation( "Vertices", [ IsDigraph ] );  
DeclareOperation( "Arcs", [ IsDigraph ] );  

############################################################################## 
## 
#R  IsFpWeightedDigraph( <dig> )                
## 
##  A FpWeightedDigraph is a set of vertices and a set of directed arcs  
## 
DeclareRepresentation( "IsFpWeightedDigraph",
    IsDigraph and IsComponentObjectRep, [ "group", "vertices", "arcs" ] ); 
##  (24/01/13) switched from IsAttributeStoringRep to IsComponentObjectRep
##    IsDigraph and IsAttributeStoringRep, [ "GroupOfFpWeightedDigraph" ] ); 
 
############################################################################# 
## 
#O  FpWeightedDigraphNC( <group>, <vertices>, <arcs> ) 
#O  FpWeightedDigraph( <group>, <vertices>, <arcs> )                                  
## 
DeclareOperation( "FpWeightedDigraphNC",  
    [ IsGroup, IsHomogeneousList, IsHomogeneousList ] );  
DeclareOperation( "FpWeightedDigraph", 
    [ IsGroup, IsHomogeneousList, IsHomogeneousList ] );    
 
############################################################################## 
## 
#A  InvolutoryArcs( <dig> )                
## 
##  An involutory digraph is a set of vertices and a set of directed arcs  
##  and an involution on the set of arcs which reverses source and target 
## 
DeclareAttribute( "InvolutoryArcs", IsFpWeightedDigraph ); 

############################################################################# 
## 
#O  WeightedAdjacencyMatrix( <dig> ) 
#O  WeightedSpanningTree( <dig> ) 
#O  ArcsIsosFromMatrices( <vertices>, <wt_adj_mx>, <isos_mx> ) 
DeclareOperation( "WeightedAdjacencyMatrix", [ IsDigraph ] ); 
DeclareOperation( "WeightedSpanningTree", [ IsFpWeightedDigraph ] ); 
DeclareOperation( "ArcsIsosFromMatrices", 
    [ IsHomogeneousList, IsHomogeneousList, IsHomogeneousList ] ); 


## ------------------------------------------------------------------------##
##                          Graphs of Groups                               ##
## ------------------------------------------------------------------------##
 
############################################################################## 
## 
#P  IsStructuredDigraph( <dig> )                
## 
DeclareProperty( "IsStructuredDigraph", IsDigraph ); 
 
############################################################################# 
## 
#C  IsGraphOfGroups( <gg> ) 
#V  GraphOfGroupsFamily
#R  IsGraphOfGroupsRep( <gg> )                                                
## 
##  A FpWeightedDigraph of groups is a 4-tuple containing 
##  - a FpWeightedDigraph,  
##  - groups associated with each vertex, 
##  - subgroup associated to tail vertex of each edge, and 
##  - an isomorphism associated to each edge.  
## 
DeclareCategory( "IsGraphOfGroups", IsDigraph ); 
BindGlobal( "GraphOfGroupsFamily", 
    NewFamily( "GraphOfGroupsFamily", IsGraphOfGroups ) ); 
DeclareRepresentation( "IsGraphOfGroupsRep",
    IsStructuredDigraph and IsAttributeStoringRep, 
    [ "DigraphOfGraphOfGroups", "GroupsOfGraphOfGroups", 
##    "SubgroupsOfGraphOfGroups", 
      "IsomorphismsOfGraphOfGroups" ] ); 
 
############################################################################## 
## 
#P  IsGraphOfPermGroups( <gg> ) 
#P  IsGraphOfFpGroups( <gg> ) 
#P  IsGraphOfPcGroups( <gg> ) 
## 
DeclareProperty( "IsGraphOfPermGroups", IsGraphOfGroups ); 
DeclareProperty( "IsGraphOfFpGroups", IsGraphOfGroups ); 
DeclareProperty( "IsGraphOfPcGroups", IsGraphOfGroups ); 
 
############################################################################# 
## 
#O  GraphOfGroupsNC( <dig>, <gps>, <isos> )                      
#O  GraphOfGroups( <dig>, <gps>, <isos> )                 
## 
DeclareOperation( "GraphOfGroupsNC", 
    [ IsFpWeightedDigraph, IsList, IsList ] );  
DeclareOperation( "GraphOfGroups", 
    [ IsFpWeightedDigraph, IsList, IsList ] );    
 
#############################################################################
## 
#A  GroupsOfGraphOfGroups( <gg> )                                        
#A  DigraphOfGraphOfGroups( <gg> )                                    
#A  SubgroupsOfGraphOfGroups( <gg> )                                       
#A  IsomorphismsOfGraphOfGroups( <gg> )                                    
#A  RightTransversalsOfGraphOfGroups( <gg> ) 
#A  LeftTransversalsOfGraphOfGroups( <gg> ) 
## 
DeclareAttribute( "GroupsOfGraphOfGroups", IsGraphOfGroups ); 
DeclareAttribute( "DigraphOfGraphOfGroups", IsGraphOfGroups );  
## DeclareAttribute( "SubgroupsOfGraphOfGroups", IsGraphOfGroups ); 
DeclareAttribute( "IsomorphismsOfGraphOfGroups", IsGraphOfGroups );  
DeclareAttribute( "RightTransversalsOfGraphOfGroups", IsGraphOfGroups );  
DeclareAttribute( "LeftTransversalsOfGraphOfGroups", IsGraphOfGroups );  
 
## ------------------------------------------------------------------------##
##                         Rewriting Functions                             ##
## ------------------------------------------------------------------------##
 
############################################################################# 
## 
#A  InverseOfIsomorphismFpSemigroup 
#A  FreeSemigroupOfKnuthBendixRewritingSystem( <kbrws> ) 
#O  NormalFormKBRWS( <group>, <word> )                                  
## 
##  creates inverse to  iso = IsomorphismFpSemigroup 
## 
DeclareAttribute( "InverseOfIsomorphismFpSemigroup", 
    IsNonSPMappingByFunctionRep ); 
DeclareAttribute( "FreeSemigroupOfKnuthBendixRewritingSystem", 
    IsKnuthBendixRewritingSystem ); 
DeclareOperation( "NormalFormKBRWS", [ IsFpGroup, IsObject ] );    
 
############################################################################# 
## 
#O  FreeProductWithAmalgamation( <grp>, <grp>, <iso> ) 
#P  IsFpaGroup( <fpgrp> )  
#A  FpaInfo( <fpa> )
## 
DeclareOperation( "FreeProductWithAmalgamation",
    [ IsGroup, IsGroup, IsGroupHomomorphism ] );    
DeclareProperty( "IsFpaGroup", IsFpGroup );
DeclareAttribute( "FpaInfo", IsFpaGroup, "mutable" );    
 
############################################################################# 
## 
#O  HnnExtension( <grp>, <iso> ) 
#P  IsHnnGroup( <fpgrp> )  
#A  HnnInfo( <fpa> )
## 
DeclareOperation( "HnnExtension", [ IsGroup, IsGroupHomomorphism ] );    
DeclareProperty( "IsHnnGroup", IsFpGroup );
DeclareAttribute( "HnnInfo", IsHnnGroup, "mutable" );    
 
############################################################################# 
## 
#A  GraphOfGroupsRewritingSystem( <fpgrp> )
#O  NormalFormGGRWS( <fpgrp>, <word> )                                  
## 
DeclareAttribute( "GraphOfGroupsRewritingSystem", IsFpGroup );
DeclareOperation( "NormalFormGGRWS", [ IsFpGroup, IsObject ] );    
 
## ------------------------------------------------------------------------##
##                      Graph of Groups Words                              ##
## ------------------------------------------------------------------------##
 
############################################################################## 
## 
#R  IsGraphOfGroupsWordRep( <ggword> )                
## 
##  A GraphOfGroupsWord is a word made from elements in the group 
##  and arcs in the digraph  
## 
DeclareRepresentation( "IsGraphOfGroupsWordRep", 
    IsGroupoidElement and IsAttributeStoringRep, 
   [ "GraphOfGroupsOfWord", "Tail", "WordOfGraphOfGroupsWord" ] ); 
 
############################################################################## 
## 
#P  IsGraphOfGroupsWord( <ggword> )                
#P  IsReducedGraphOfGroupsWord( <ggword> )                
##  A GraphOfGroupsWord is Reduced if of the form [ t, h, t, h, ..., t, g ] 
## 
DeclareProperty( "IsGraphOfGroupsWord", IsGraphOfGroupsWordRep ); 
DeclareProperty( "IsReducedGraphOfGroupsWord", IsGraphOfGroupsWord ); 
 
############################################################################# 
## 
#O  GraphOfGroupsWordNC( <gg>, <tv>, <wL> )  
#O  GraphOfGroupsWord( <gg>, <tv>, <wL> )                                  
## 
DeclareOperation( "GraphOfGroupsWordNC", [ IsGraphOfGroups, IsInt, IsList ] );
DeclareOperation( "GraphOfGroupsWord", [ IsGraphOfGroups, IsInt, IsList ] );   
 
############################################################################# 
## 
#A  GraphOfGroupsOfWord( <ggword> ) 
#?  GGTail, GGHead added temporarity (08/04/08)
#A  GGTail( <ggword> ) 
#A  GGHead( <ggword> ) 
#A  WordOfGraphOfGroupsWord( <ggword> )                                       
#O  ReducedGraphOfGroupsWord( <ggword> )    
## 
DeclareAttribute( "GraphOfGroupsOfWord", IsGraphOfGroupsWordRep ); 
DeclareAttribute( "GGTail", IsGraphOfGroupsWordRep ); 
DeclareAttribute( "GGHead", IsGraphOfGroupsWordRep ); 
DeclareAttribute( "WordOfGraphOfGroupsWord", IsGraphOfGroupsWordRep); 
DeclareOperation( "ReducedGraphOfGroupsWord", [ IsGraphOfGroupsWordRep ] );   
 
## ------------------------------------------------------------------------##
##         Graph of Groups Groupoid - still be to implemented              ##
## ------------------------------------------------------------------------##
 
############################################################################# 
## 
## #P  IsGraphOfGroupsGroupoid( <gpd> )  
## #O  GraphOfGroupsGroupoid( <gg> ) 
## 
## DeclareProperty( "IsGraphOfGroupsGroupoid", IsGroupoid );
## DeclareOperation( "GraphOfGroupsGroupoid", [ IsGraphOfGroups ] );
 
############################################################################# 
## 
#E  grpgraph.gd . . . . . . . . . . . . . . . . . . . . . . . . . . ends here 
## 
