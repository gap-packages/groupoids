############################################################################
##
#W  grpgraph.gd             GAP4 package `groupoids'           Chris Wensley
#W                                                              & Emma Moore
##  
##  This file contains the declarations for involutory FpWeightedDigraphs 
##  and FpWeightedDigraphs of groups, 
##  and normal forms for FpWeightedDigraph of groups. 

############################################################################## 
## 
#C  IsGroupoidDigraph( <dig> )                
## 
DeclareCategory( "IsGroupoidDigraph", IsDomain );

############################################################################# 
## 
#O  GroupoidVertices( <dig> );  
#O  GroupoidArcs( <dig> );  
## 
##  Vertices must be declared as Operation, and not Attribute, 
##  so as not to conflict with Vertices in the Grape package. 
##   
DeclareOperation( "GroupoidVertices", [ IsGroupoidDigraph ] );  
DeclareOperation( "GroupoidArcs", [ IsGroupoidDigraph ] );  

############################################################################## 
## 
#R  IsFpWeightedDigraphRep( <dig> )                
#V  IsFpWeightedDigraphFamily( <dig> )                
#T  IsFpWeightedDigraphType( <dig> )                
#P  IsFpWeightedDigraph( <dig> )                
## 
##  A FpWeightedDigraph is a set of vertices and a set of directed arcs  
## 
DeclareRepresentation( "IsFpWeightedDigraphRep",
    IsGroupoidDigraph and IsComponentObjectRep, 
    [ "group", "vertices", "arcs" ] ); 
DeclareProperty( "IsFpWeightedDigraph", IsGroupoidDigraph ); 
BindGlobal( "IsFpWeightedDigraphFamily", 
            NewFamily( "IsFpWeightedDigraphFamily", IsFpWeightedDigraphRep ) ); 
BindGlobal( "IsFpWeightedDigraphType", 
            NewType( IsFpWeightedDigraphFamily, IsFpWeightedDigraphRep ) ); 
 
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
#O  FpWeightedAdjacencyMatrix( <dig> ) 
#O  ArcsIsosFromMatrices( <vertices>, <wt_adj_mx>, <isos_mx> ) 
DeclareOperation( "FpWeightedAdjacencyMatrix", [ IsGroupoidDigraph ] ); 
DeclareOperation( "ArcsIsosFromMatrices", 
    [ IsHomogeneousList, IsHomogeneousList, IsHomogeneousList ] ); 


## ------------------------------------------------------------------------##
##                          Graphs of Groups                               ##
## ------------------------------------------------------------------------##
 
############################################################################## 
## 
#P  IsStructuredDigraph( <dig> )                
## 
DeclareProperty( "IsStructuredDigraph", IsGroupoidDigraph ); 
 
############################################################################# 
## 
#C  IsGraphOfGroups( <gg> ) 
#R  IsGraphOfGroupsRep( <gg> )                                                
#V  IsGraphOfGroupsFamily
#T  IsGraphOfGroupsType
## 
##  A FpWeightedDigraph of groups is a 4-tuple containing 
##  - a FpWeightedDigraph,  
##  - groups associated with each vertex, 
##  - subgroup associated to tail vertex of each edge, and 
##  - an isomorphism associated to each edge.  
## 
DeclareCategory( "IsGraphOfGroups", IsGroupoidDigraph ); 
DeclareRepresentation( "IsGraphOfGroupsRep",
    IsStructuredDigraph and IsAttributeStoringRep, 
    [ "DigraphOfGraphOfGroups", "GroupsOfGraphOfGroups", 
      #?  "SubgroupsOfGraphOfGroups", 
      "IsomorphismsOfGraphOfGroups" ] ); 
BindGlobal( "IsGraphOfGroupsFamily", 
    NewFamily( "IsGraphOfGroupsFamily", IsGraphOfGroups ) ); 
BindGlobal( "IsGraphOfGroupsType", 
    NewType( IsGraphOfGroupsFamily, IsGraphOfGroupsRep ) ); 
 
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
#A  FreeSemigroupOfKnuthBendixRewritingSystem( <kbrws> ) 
#O  NormalFormKBRWS( <group>, <word> )                                  
## 
##  creates inverse to  iso = IsomorphismFpSemigroup 
## 
DeclareAttribute( "FreeSemigroupOfKnuthBendixRewritingSystem", 
    IsKnuthBendixRewritingSystem ); 
DeclareOperation( "NormalFormKBRWS", [ IsFpGroup, IsObject ] );    
 
############################################################################# 
## 
#O  FreeProductWithAmalgamation( <grp>, <grp>, <iso> ) 
#O  FreeProductWithAmalgamationOp( <grp>, <grp>, <iso> ) 
#A  FreeProductWithAmalgamationInfo( <fpagrp> )
#P  IsFreeProductWithAmalgamation( <fpgrp> )  
## 
DeclareOperation( "FreeProductWithAmalgamation",
    [ IsGroup, IsGroup, IsGroupHomomorphism ] );    
DeclareOperation( "FreeProductWithAmalgamationOp",
    [ IsGroup, IsGroup, IsGroupHomomorphism ] );    
DeclareProperty( "IsFreeProductWithAmalgamation", IsFpGroup );
DeclareAttribute( "FreeProductWithAmalgamationInfo", 
    IsFreeProductWithAmalgamation, "mutable" );    
 
############################################################################# 
## 
#O  HnnExtension( <grp>, <iso> ) 
#P  IsHnnExtension( <fpgrp> )  
#A  HnnExtensionInfo( <fpa> )
## 
DeclareOperation( "HnnExtension", [ IsGroup, IsGroupHomomorphism ] );    
DeclareProperty( "IsHnnExtension", IsFpGroup );
DeclareAttribute( "HnnExtensionInfo", IsHnnExtension, "mutable" );    
 
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
#V  IsGraphOfGroupsWordFamily( <ggword> )                
#T  IsGraphOfGroupsWordType( <ggword> )                
## 
##  A GraphOfGroupsWord is a word made from elements in the group 
##  and arcs in the digraph  
## 
DeclareRepresentation( "IsGraphOfGroupsWordRep", 
    ## IsGroupoidElement and IsAttributeStoringRep, 
    IsObject and IsAttributeStoringRep, 
    [ "GraphOfGroupsOfWord", 
      "TailOfGraphOfGroupsWord", "WordOfGraphOfGroupsWord" ] ); 
BindGlobal( "IsGraphOfGroupsWordFamily", 
            NewFamily( "IsGraphOfGroupsWordFamily", 
                       IsGraphOfGroupsWordRep ) );
BindGlobal( "IsGraphOfGroupsWordType", 
            NewType( IsGraphOfGroupsWordFamily, IsGraphOfGroupsWordRep ) );
 
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
#A  HeadOfGraphOfGroupsWord( <ggword> ) 
#A  TailOfGraphOfGroupsWord( <ggword> ) 
#A  WordOfGraphOfGroupsWord( <ggword> )                                       
#O  ReducedGraphOfGroupsWord( <ggword> )    
## 
DeclareAttribute( "GraphOfGroupsOfWord", IsGraphOfGroupsWordRep ); 
DeclareAttribute( "HeadOfGraphOfGroupsWord", IsGraphOfGroupsWordRep ); 
DeclareAttribute( "TailOfGraphOfGroupsWord", IsGraphOfGroupsWordRep ); 
DeclareAttribute( "WordOfGraphOfGroupsWord", IsGraphOfGroupsWordRep); 
DeclareOperation( "ReducedGraphOfGroupsWord", [ IsGraphOfGroupsWordRep ] );   
 
############################################################################# 
## 
#P  IsMappingToGroupWithGGRWS( <map> ) 
#O  ReducedImageElm( <hom>, <elm> )    
## 
DeclareProperty( "IsMappingToGroupWithGGRWS", 
    IsGroupGeneralMappingByImages ); 
DeclareOperation( "ReducedImageElm", [ IsMappingToGroupWithGGRWS, 
    IsMultiplicativeElementWithInverse ] );   


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
