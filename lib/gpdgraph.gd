############################################################################
##
#W  gpdgraph.gd             GAP4 package `groupoids'           Chris Wensley
#W                                                              & Emma Moore

############################################################################# 
## 
#C  IsGraphOfGroupoids( <gg> ) 
#R  IsGraphOfGroupoidsRep( <gg> ) 
#V  GraphOfGroupoidsFamily
#T  GraphOfGroupoidsType 
## 
##  A FpWeightedDigraph of groupoids is a 4-tuple containing 
##  - a FpWeightedDigraph,  
##  - groups associated with each vertex, 
##  - subgroupoid associated to tail vertex of each edge, and 
##  - an isomorphism associated to each edge.  
## 
DeclareCategory( "IsGraphOfGroupoids", IsGroupoidDigraph ); 
DeclareRepresentation( "IsGraphOfGroupoidsRep",
    IsStructuredDigraph and IsAttributeStoringRep and IsComponentObjectRep,
    [ "DigraphOfGraphOfGroupoids", "GroupoidsOfGraphOfGroupoids", 
      "SubgroupoidsOfGraphOfGroupoids", "IsomorphismsOfGraphOfGroupoids" ] ); 
BindGlobal( "GraphOfGroupoidsFamily", 
    NewFamily( "GraphOfGroupoidsFamily", IsGraphOfGroupoids ) ); 
BindGlobal( "GraphOfGroupoidsType", 
            NewType( GraphOfGroupoidsFamily, IsGraphOfGroupoidsRep ) ); 


############################################################################## 
## 
#P  IsGraphOfPermGroupoids( <gg> ) 
## 
DeclareProperty( "IsGraphOfPermGroupoids", IsGraphOfGroupoids ); 
 
############################################################################## 
## 
#P  IsGraphOfFpGroupoids( <gg> ) 
#P  IsGraphOfPcGroupoids( <gg> ) 
## 
DeclareProperty( "IsGraphOfFpGroupoids", IsGraphOfGroupoids ); 
DeclareProperty( "IsGraphOfPcGroupoids", IsGraphOfGroupoids ); 
 
############################################################################# 
## 
#O  GraphOfGroupoidsNC( <dig>, <gps>, <subgps>, <isos> )                      
#O  GraphOfGroupoids( <dig>, <gpds>, <subgpds>, <isos> )                 
## 
DeclareOperation( "GraphOfGroupoidsNC", 
    [ IsFpWeightedDigraph, IsList, IsList, IsList ] );  
DeclareOperation( "GraphOfGroupoids", 
    [ IsFpWeightedDigraph, IsList, IsList, IsList ] );    
 
############################################################################# 
## 
#A  GroupoidsOfGraphOfGroupoids( <gg> )
#A  DigraphOfGraphOfGroupoids( <gg> )                                    
#A  SubgroupoidsOfGraphOfGroupoids( <gg> ) 
#A  IsomorphismsOfGraphOfGroupoids( <gg> )
## 
DeclareAttribute( "GroupoidsOfGraphOfGroupoids", IsGraphOfGroupoids ); 
DeclareAttribute( "DigraphOfGraphOfGroupoids", IsGraphOfGroupoids );  
DeclareAttribute( "SubgroupoidsOfGraphOfGroupoids", IsGraphOfGroupoids ); 
DeclareAttribute( "IsomorphismsOfGraphOfGroupoids", IsGraphOfGroupoids );  
 
############################################################################# 
## 
#O  NormalFormKBRWS( <groupoid>, <word> )                                  
## 
##  DeclareOperation( "NormalFormKBRWS", [ IsFpGroupoid, IsObject ] );    
 
############################################################################# 
## 
#A  RightTransversalsOfGraphOfGroupoids( <gg> ) 
#A  LeftTransversalsOfGraphOfGroupoids( <gg> ) 
## 
DeclareAttribute( "RightTransversalsOfGraphOfGroupoids", IsGraphOfGroupoids );
DeclareAttribute( "LeftTransversalsOfGraphOfGroupoids", IsGraphOfGroupoids );  
 
######################################### #################################### 
## 
#R  IsGraphOfGroupoidsWordRep( <ggword> )                
#V  IsGraphOfGroupoidsWordFamily( <ggword> )                
#T  IsGraphOfGroupoidsWordType( <ggword> )                
## 
##  A GraphOfGroupoidsWord is a word made from elements in the groupoid 
##  and edges in the digraph  
## 
DeclareRepresentation( "IsGraphOfGroupoidsWordRep", 
    IsObject and IsAttributeStoringRep and IsComponentObjectRep, 
   [ "GraphOfGroupoidsOfWord", 
     "TailOfGraphOfGroupsWord", "WordOfGraphOfGroupoidsWord" ] ); 
BindGlobal( "IsGraphOfGroupoidsWordFamily", 
            NewFamily( "IsGraphOfGroupoidsWordFamily", 
                       IsGraphOfGroupoidsWordRep ) ); 
BindGlobal( "IsGraphOfGroupoidsWordType", 
            NewType( IsGraphOfGroupoidsWordFamily, 
                     IsGraphOfGroupoidsWordRep ) );
 
######################################### #################################### 
## 
#P  IsGraphOfGroupoidsWord( <ggword> )                
#P  IsReducedGraphOfGroupoidsWord( <ggword> ) 
##  A GraphOfGroupoidsWord is Reduced if of the form [ t, h, t, h, ..., t, g ] 
## 
DeclareProperty( "IsGraphOfGroupoidsWord", IsGraphOfGroupoidsWordRep ); 
DeclareProperty( "IsReducedGraphOfGroupoidsWord", IsGraphOfGroupoidsWord ); 
 
############################################################################# 
## 
#O  GraphOfGroupoidsWordNC( <gg>, <tv>, <wL> )  
#O  GraphOfGroupoidsWord( <gg>, <tv>, <wL> )                                  
## 
DeclareOperation( "GraphOfGroupoidsWordNC",  
    [ IsGraphOfGroupoids, IsInt, IsList ] );  
DeclareOperation( "GraphOfGroupoidsWord", 
    [ IsGraphOfGroupoids, IsInt, IsList ] );   
 
############################################################################# 
## 
#A  GraphOfGroupoidsOfWord( <ggword> ) 
#A  WordOfGraphOfGroupoidsWord( <ggword> ) 
#O  ReducedGraphOfGroupoidsWord( <ggword> )    
## 
DeclareAttribute( "GraphOfGroupoidsOfWord", IsGraphOfGroupoidsWordRep ); 
DeclareAttribute( "WordOfGraphOfGroupoidsWord", IsGraphOfGroupoidsWordRep); 
DeclareOperation( "ReducedGraphOfGroupoidsWord",
    [ IsGraphOfGroupoidsWordRep ] );   
 
############################################################################# 
## 
#P  IsGraphOfGroupoidsGroupoid( <gpd> )  
#O  GraphOfGroupoidsGroupoid( <gg> ) 
## 
DeclareOperation( "IsGraphOfGroupoidsGroupoid", [ IsGroupoid ] );
DeclareOperation( "GraphOfGroupoidsGroupoid", [ IsGraphOfGroupoids ] );
