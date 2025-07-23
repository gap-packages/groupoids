############################################################################
##
#W  ggraph.tst              groupoids Package                  Chris Wensley
#W                                                              & Emma Moore

gap> START_TEST( "groupoids package: ggraph.tst" );
gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

###  Section 7.1  Digraphs

## SubSection 7.1.1
## define the graph D1
gap> V1 := [5,6];;
gap> fg1 := FreeGroup("y");;
gap> y := fg1.1;;
gap> A1 := [ [y,5,6], [y^-1,6,5]];;
gap> D1 := FpWeightedDigraph( fg1, V1, A1 );
weighted digraph with vertices: [ 5, 6 ]
and arcs: [ [ y, 5, 6 ], [ y^-1, 6, 5 ] ]
gap> inv1 := InvolutoryArcs( D1 );
[ 2, 1 ]

## define the graph D3
gap> fg3 := FreeGroup(3,"z");;
gap> z1:=fg3.1;; z2:=fg3.2;; z3:=fg3.3;;
gap> ob3 := [7,8,9];;
gap> A3 := [[z1,7,8],[z2,8,9],[z3,9,7],[z1^-1,8,7],[z2^-1,9,8],[z3^-1,7,9]];;
gap> D3 := FpWeightedDigraph( fg3, ob3, A3 );
weighted digraph with vertices: [ 7, 8, 9 ]
and arcs: [ [ z1, 7, 8 ], [ z2, 8, 9 ], [ z3, 9, 7 ], [ z1^-1, 8, 7 ], 
  [ z2^-1, 9, 8 ], [ z3^-1, 7, 9 ] ]
gap> inob3 := InvolutoryArcs( D3 );
[ 4, 5, 6, 1, 2, 3 ]

### Section 7.2 : Graphs of Groups

## SubSection 7.2.1
## group fa and subgroup 3fa
gap> fa := FreeGroup( "a" );;
gap> a := fa.1;;
gap> SetName( fa, "fa" );
gap> hy := Subgroup( fa, [a^3] );;
gap> SetName( hy, "hy" );

## group fb and subgroup 2fb
gap> fb := FreeGroup( "b" );;
gap> b := fb.1;;
gap> SetName( fb, "fb" );
gap> hybar := Subgroup( fb, [b^2] );;
gap> SetName( hybar, "hybar" );

## Isomorphisms between subgroups 
gap> homy := GroupHomomorphismByImagesNC( hy, hybar, [a^3], [b^2] );;
gap> homybar := GroupHomomorphismByImagesNC( hybar, hy, [b^2], [a^3] );; 

## defining graph of groups 
gap> G1 := GraphOfGroups( D1, [fa,fb], [homy,homybar] );
Graph of Groups: 2 vertices; 2 arcs; groups [ fa, fb ]
gap> Display( G1 );
Graph of Groups with :- 
    vertices: [ 5, 6 ]
        arcs: [ [ y, 5, 6 ], [ y^-1, 6, 5 ] ]
      groups: [ fa, fb ]
isomorphisms: [ [ [ a^3 ], [ b^2 ] ], [ [ b^2 ], [ a^3 ] ] ]
gap> IsGraphOfGroups( G1 );
true

## SubSection 7.2.2
gap> IsGraphOfFpGroups(G1);
true
gap> IsomorphismsOfGraphOfGroups(G1);
[ [ a^3 ] -> [ b^2 ], [ b^2 ] -> [ a^3 ] ]

## SubSection 7.2.3
gap> RTG1 := RightTransversalsOfGraphOfGroups( G1 );
[ [ <identity ...>, a, a^2 ], [ <identity ...>, b ] ]
gap> LTG1 := LeftTransversalsOfGraphOfGroups( G1 );
[ [ <identity ...>, a^-1, a^-2 ], [ <identity ...>, b^-1 ] ]

### Section 7.3  Words in a Graph of Groups and their normal forms

## SubSection 7.3.1
gap> L1 := [ a^7, 1, b^-6, 2, a^-11, 1, b^9, 2, a^7 ];;
gap> gw1 := GraphOfGroupsWord( G1, 5, L1 );
(5)a^7.y.b^-6.y^-1.a^-11.y.b^9.y^-1.a^7(5)
gap> IsGraphOfGroupsWord( gw1 );
true
gap> [ TailOfGraphOfGroupsWord( gw1 ), HeadOfGraphOfGroupsWord( gw1 ) ];
[ 5, 5 ]
gap> GraphOfGroupsOfWord( gw1 );
Graph of Groups: 2 vertices; 2 arcs; groups [ fa, fb ]
gap> WordOfGraphOfGroupsWord( gw1 );
[ a^7, 1, b^-6, 2, a^-11, 1, b^9, 2, a^7 ]

## SubSection 7.3.2
gap> nw1 := ReducedGraphOfGroupsWord( gw1 );
(5)a^-1.y.b^-1.y^-1.a^10(5)

### Section 7.4 : Free products with amalgamation and HNN extensions

## Subsection 7.4.1
## set up the first group s3 and a subgroup c3=<a1>
gap> fg2 := FreeGroup( 2, "a" );;
gap> rel1 := [fg2.1^3, fg2.2^2, (fg2.1*fg2.2)^2];;
gap> s3 := fg2/rel1;;
gap> gs3 := GeneratorsOfGroup(s3);;
gap> SetName(s3,"s3");
gap> a1:=gs3[1];;  a2:=gs3[2];;
gap> H1 := Subgroup(s3,[a1]);;

## then the second group a4 and subgroup c3=<b1>
gap> f2 := FreeGroup( 2, "b" );;
gap> rel2 := [f2.1^3, f2.2^3, (f2.1*f2.2)^2];;
gap> a4 := f2/rel2;;
gap> ga4 := GeneratorsOfGroup(a4);;
gap> SetName(a4,"a4");
gap> b1 := ga4[1];;  b2:=ga4[2];;
gap> H2 := Subgroup(a4,[b1]);;
gap> ## form the isomorphism and the fpa group
gap> iso := GroupHomomorphismByImages(H1,H2,[a1],[b1]);;
gap> inv := InverseGeneralMapping(iso);;
gap> fpa := FreeProductWithAmalgamation( s3, a4, iso );
<fp group on the generators [ f1, f2, f3, f4 ]>
gap> RelatorsOfFpGroup( fpa );
[ f1^2, f2^3, (f2*f1)^2, f3^3, f4^3, (f4*f3)^2, f2*f3^-1 ]
gap> gg1 := GraphOfGroupsRewritingSystem( fpa );;
gap> Display( gg1 );
Graph of Groups with :- 
    vertices: [ 5, 6 ]
        arcs: [ [ y, 5, 6 ], [ y^-1, 6, 5 ] ]
      groups: [ s3, a4 ]
isomorphisms: [ [ [ a1 ], [ b1 ] ], [ [ b1 ], [ a1 ] ] ]
gap> LeftTransversalsOfGraphOfGroups( gg1 );
[ [ <identity ...>, a2^-1 ], [ <identity ...>, b2^-1, b1^-1*b2^-1, b1*b2^-1 ] 
 ]
gap> gfpa := GeneratorsOfGroup( fpa );;
gap> w2 := (gfpa[1]*gfpa[2]*gfpa[3]^gfpa[4])^3;
(f1*f2*f4^-1*f3*f4)^3
gap> n2 := NormalFormGGRWS( fpa, w2 );
f2*f3*(f4^-1*f2)^2*f4^-1*f3
gap> ##
gap> fpainfo := FreeProductWithAmalgamationInfo( fpa );
rec( embeddings := [ [ a2, a1 ] -> [ f1, f2 ], [ b1, b2 ] -> [ f3, f4 ] ], 
  groups := [ s3, a4 ], isomorphism := [ a1 ] -> [ b1 ], 
  positions := [ [ 1, 2 ], [ 3, 4 ] ], 
  subgroups := [ Group([ a1 ]), Group([ b1 ]) ] )
gap> emb2 := Embedding( fpa, 2 );
[ b1, b2 ] -> [ f3, f4 ]
gap> ImageElm( emb2, b1^b2 );       
f4^-1*f3*f4
gap> ReducedImageElm( emb2, b1^b2 );
f4*f3^-1

## Subsection 7.4.2
gap> H3 := Subgroup(a4,[b2]);;
gap> i23 := GroupHomomorphismByImages( H2, H3, [b1], [b2] );;
gap> hnn := HnnExtension( a4, i23 );
<fp group of size infinity on the generators [ fe1, fe2, fe3 ]>
gap> phnn := PresentationFpGroup( hnn );;
gap> TzPrint( phnn );
#I  generators: [ fe1, fe2, fe3 ]
#I  relators:
#I  1.  3  [ 1, 1, 1 ]
#I  2.  3  [ 2, 2, 2 ]
#I  3.  4  [ 1, 2, 1, 2 ]
#I  4.  4  [ -3, 1, 3, -2 ]
gap> gg2 := GraphOfGroupsRewritingSystem( hnn );
Graph of Groups: 1 vertices; 2 arcs; groups [ a4 ]
gap> LeftTransversalsOfGraphOfGroups( gg2 );
[ [ <identity ...>, b2^-1, b1^-1*b2^-1, b1*b2^-1 ], 
  [ <identity ...>, b1^-1, b1, b2^-1*b1 ] ]
gap> gh := GeneratorsOfGroup( hnn );;
gap> w3 := (gh[1]^gh[2])*gh[3]^-1*(gh[1]*gh[3]*gh[2]^2)^2*gh[3]*gh[2];
fe2^-1*fe1*fe2*fe3^-1*(fe1*fe3*fe2^2)^2*fe3*fe2
gap> n3 := NormalFormGGRWS( hnn, w3 );
(fe2*fe1*fe3)^2
gap> ##
gap> hnninfo := HnnExtensionInfo( hnn );
rec( embeddings := [ [ b1, b2 ] -> [ fe1, fe2 ] ], group := a4, 
  isomorphism := [ b1 ] -> [ b2 ], 
  subgroups := [ Group([ b1 ]), Group([ b2 ]) ] )
gap> emb := Embedding( hnn, 1 );
[ b1, b2 ] -> [ fe1, fe2 ]
gap> ImageElm( emb, b1^b2 );       
fe2^-1*fe1*fe2
gap> ReducedImageElm( emb, b1^b2 );
fe2*fe1^-1

### Section 7.5 : Graphs of groupoids and their words 

## Subsection 7.5.1
gap> Gfa := SinglePieceGroupoid( fa, [-2,-1] );;
gap> ofa := One( fa );;
gap> SetName( Gfa, "Gfa" );
gap> Uhy := Subgroupoid( Gfa, [ [ hy, [-2,-1] ] ] );;
gap> SetName( Uhy, "Uhy" );
gap> Gfb := SinglePieceGroupoid( fb, [-4,-3] );;
gap> ofb := One( fb );;
gap> SetName( Gfb, "Gfb" );
gap> Uhybar := Subgroupoid( Gfb, [ [ hybar, [-4,-3] ] ] );;
gap> SetName( Uhybar, "Uhybar" );
gap> gens := GeneratorsOfGroupoid( Uhy );; 
gap> gensbar := GeneratorsOfGroupoid( Uhybar );;
gap> mory := GroupoidHomomorphismFromSinglePiece( 
>                Uhy, Uhybar, gens, gensbar );
groupoid homomorphism : Uhy -> Uhybar
[ [ [a^3 : -2 -> -2], [<identity ...> : -2 -> -1] ], 
  [ [b^2 : -4 -> -4], [<identity ...> : -4 -> -3] ] ]
gap> morybar := InverseGeneralMapping( mory );
groupoid homomorphism : Uhybar -> Uhy
[ [ [b^2 : -4 -> -4], [<identity ...> : -4 -> -3] ], 
  [ [a^3 : -2 -> -2], [<identity ...> : -2 -> -1] ] ]
gap> gg3 := GraphOfGroupoids( D1, [Gfa,Gfb], [Uhy,Uhybar], [mory,morybar] );;
gap> Display( gg3 );
Graph of Groupoids with :- 
    vertices: [ 5, 6 ]
        arcs: [ [ y, 5, 6 ], [ y^-1, 6, 5 ] ]
   groupoids: 
fp single piece groupoid: Gfa
  objects: [ -2, -1 ]
    group: fa = <[ a ]>
fp single piece groupoid: Gfb
  objects: [ -4, -3 ]
    group: fb = <[ b ]>
subgroupoids: single piece groupoid: Uhy
  objects: [ -2, -1 ]
    group: hy = <[ a^3 ]>
single piece groupoid: Uhybar
  objects: [ -4, -3 ]
    group: hybar = <[ b^2 ]>
isomorphisms: [ groupoid homomorphism : Uhy -> Uhybar
    [ [ [a^3 : -2 -> -2], [<identity ...> : -2 -> -1] ], 
      [ [b^2 : -4 -> -4], [<identity ...> : -4 -> -3] ] ], 
  groupoid homomorphism : Uhybar -> Uhy
    [ [ [b^2 : -4 -> -4], [<identity ...> : -4 -> -3] ], 
      [ [a^3 : -2 -> -2], [<identity ...> : -2 -> -1] ] ] ]
gap> IsGraphOfGroupoids( gg3 );
true

## Subsection 7.5.2
gap> f1 := Arrow( Gfa, a^7, -1, -2);;
gap> f2 := Arrow( Gfb, b^-6, -4, -4 );;
gap> f3 := Arrow( Gfa, a^-11, -2, -1 );;
gap> f4 := Arrow( Gfb, b^9, -3, -4 );;
gap> f5 := Arrow( Gfa, a^7, -2, -2 );;
gap> L3 := [ f1, 1, f2, 2, f3, 1, f4, 2, f5 ];
[ [a^7 : -1 -> -2], 1, [b^-6 : -4 -> -4], 2, [a^-11 : -2 -> -1], 1, 
  [b^9 : -3 -> -4], 2, [a^7 : -2 -> -2] ]
gap> gw3 := GraphOfGroupoidsWord( gg3, 5, L3);
(5)[a^7 : -1 -> -2].y.[b^-6 : -4 -> -4].y^-1.[a^-11 : -2 -> -1].y.[b^9 : 
-3 -> -4].y^-1.[a^7 : -2 -> -2](5)
gap> nw3 := ReducedGraphOfGroupoidsWord( gw3 );
(5)[a^-1 : -1 -> -1].y.[b^-1 : -3 -> -3].y^-1.[a^10 : -1 -> -2](5)
gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );;  
gap> STOP_TEST( "ggraph.tst", 10000 );
