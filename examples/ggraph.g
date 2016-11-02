##############################################################################
##
#W  ggraph.g                    GAP4 package `Gpd'               Chris Wensley
#W                                                                & Emma Moore
#Y  Copyright (C) 2000-2016, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 

SetInfoLevel( InfoGpd, 2 );

Print( "\n==========================================================\n");
Print(   "<<<<      testing examples in the Gpd manual          >>>>\n" );
Print(   "<<<< functions for Graphs of Groups and Groupoids     >>>>\n" );
Print( "============================================================\n\n");

## now arcs in the order  [ wt, tail, head ]

## define the graph D1
V1 := [5,6];
fg1 := FreeGroup("y");
y := fg1.1;
A1 := [ [y,5,6], [y^-1,6,5]];
D1 := FpWeightedDigraph( fg1, V1, A1 );
Print( D1!.vertices, "\n", D1!.arcs, "\n" );
## Print( GpdVertices( D1 ), "\n", GpdArcs( D1 ), "\n" );
# Display( D1 );
inv1 := InvolutoryArcs( D1 );
Print( inv1, "\n\n" );

## define the graph D3
fg3 := FreeGroup(3,"z");
Print( fg3, "\n" );
gfg3 := GeneratorsOfGroup(fg3);
Print( gfg3, "\n" );
z1:=gfg3[1];; z2:=gfg3[2];; z3:=gfg3[3];;
V3 := [7,8,9];
Print( V3, "\n" );
A3 := [[z1,7,8],[z2,8,9],[z3,9,7],[z1^-1,8,7],[z2^-1,9,8],[z3^-1,7,9]];
Print( A3, "\n" );
D3 := FpWeightedDigraph( fg3, V3, A3 );
Print( D3, "\n" );
inv3 := InvolutoryArcs( D3 );
Print( inv3, "\n\n" );

## group fa and subgroup 3fa
fa := FreeGroup( "a" );;
a := GeneratorsOfGroup( fa )[1];;
SetName( fa, "fa" );
ha := Subgroup( fa, [a^3] );;
SetName( ha, "ha" );

## group fb and subgroup 2fb
fb := FreeGroup( "b" );;
b := GeneratorsOfGroup( fb )[1];;
SetName( fb, "fb" );
hb := Subgroup( fb, [b^2] );;
SetName( hb, "hb" );

## group fc and subgroup 5fc
fc := FreeGroup( "c" );;
c := GeneratorsOfGroup( fc )[1];;
SetName( fc, "fc" );
hc := Subgroup( fc, [c^5] );;
SetName( hc, "hc" );

## Isomorphisms between subgroups 
homab := GroupHomomorphismByImagesNC( ha, hb, [a^3], [b^2] );;
homba := GroupHomomorphismByImagesNC( hb, ha, [b^2], [a^3] );; 
homac := GroupHomomorphismByImagesNC( ha, hc, [a^3], [c^5] );;
homca := GroupHomomorphismByImagesNC( hc, ha, [c^5], [a^3] );; 
hombc := GroupHomomorphismByImagesNC( hb, hc, [b^2], [c^5] );;
homcb := GroupHomomorphismByImagesNC( hc, hb, [c^5], [b^2] );; 

## defining graph of groups G1 
G1 := GraphOfGroups( D1, [fa,fb], [homab,homba] );
Display( G1 ); 
Print( "\nIsGraphOfFpGroups(G1)? ", IsGraphOfFpGroups(G1), "\n" );
Print( "G1 has isomorphisms:\n", IsomorphismsOfGraphOfGroups(G1), "\n\n" );

RTG1 := RightTransversalsOfGraphOfGroups( G1 );
Print( "right transversals = \n", RTG1, "\n" );
LTG1 := LeftTransversalsOfGraphOfGroups( G1 );
Print( "left transversals = \n", LTG1, "\n\n" );

L1 := [ a^7, 1, b^-6, 2, a^-11, 1, b^9, 2, a^7 ];;
gw1 := GraphOfGroupsWord( G1, 5, L1 );
Print( gw1, "\n");
Print( IsGraphOfGroupsWord( gw1 ), "\n" );
Print( [ GGTail( gw1 ), GGHead( gw1 ) ], "\n" );
Print( GraphOfGroupsOfWord( gw1 ), "\n" );
Print( WordOfGraphOfGroupsWord( gw1 ), "\n\n" );

nw1 := ReducedGraphOfGroupsWord( gw1 );
Print( nw1, "\n\n");

## defining graph of groups G3 
G3 := GraphOfGroups( D3, [fa,fb,fc], [homab,hombc,homca,homba,homcb,homac,] );
Display( G3 ); 
Print( "\nIsGraphOfFpGroups(G3)? ", IsGraphOfFpGroups(G3), "\n" );
Print( "G3 has isomorphisms:\n", IsomorphismsOfGraphOfGroups(G3), "\n\n" );

RTG3 := RightTransversalsOfGraphOfGroups( G3 );
Print( "right transversals = \n", RTG3, "\n" );
LTG3 := LeftTransversalsOfGraphOfGroups( G3 );
Print( "left transversals = \n", LTG3, "\n\n" );

L3 := [ a^7, 1, b^-6, 2, c^-11, 3, a^9, 6, c^7 ];;
gw3 := GraphOfGroupsWord( G3, 7, L3 );
Print( "gw3 = ", gw3, "\n");
Print( IsGraphOfGroupsWord( gw3 ), "\n" );
Print( [ GGTail( gw3 ), GGHead( gw3 ) ], "\n" );
Print( GraphOfGroupsOfWord( gw3 ), "\n" );
Print( WordOfGraphOfGroupsWord( gw3 ), "\n\n" );

nw3 := ReducedGraphOfGroupsWord( gw3 );
Print( "nw3 = ", nw3, "\n\n");

L4 := [ c^5, 5, b^3, 4, a^-5 ];;
gw4 := GraphOfGroupsWord( G3, 9, L4 );
Print( "gw4 = ", gw4, "\n");
Print( IsGraphOfGroupsWord( gw4 ), "\n" );
Print( [ GGTail( gw4 ), GGHead( gw4 ) ], "\n" );
Print( GraphOfGroupsOfWord( gw4 ), "\n" );
Print( WordOfGraphOfGroupsWord( gw4 ), "\n\n" );

nw4 := ReducedGraphOfGroupsWord( gw4 );
Print( "nw4 = ", nw4, "\n\n");

inw4 := nw4^-1; 
Print( "inw4 = nw4^-1 = ", inw4, "\n\n" ); 
nw34 := nw3*nw4;
Print( "nw34 = ", nw34, "\n\n");

##############################################################################

Print( "\n===============================================================\n" );
Print( "<<<<           functions for FPAs and HNNs                 >>>>\n" );
Print( "===============================================================\n\n" );

fg2 := FreeGroup( 2, "a" );
Print( fg2, "\n" );
rel1 := [fg2.1^3, fg2.2^2, (fg2.1*fg2.2)^2];
Print( rel1, "\n" );
s3 := fg2/rel1;
Print( s3, "\n" );
gs3 := GeneratorsOfGroup(s3);
Print( gs3, "\n" );
SetName(s3,"s3");
a1:=gs3[1];  a2:=gs3[2];
Print( a1, "\n", a2, "\n" );
H1 := Subgroup(s3,[a1]);
Print( H1, "\n" );
f2 := FreeGroup( 2, "b" );
Print( f2, "\n" );
rel2 := [f2.1^3, f2.2^3, (f2.1*f2.2)^2];
Print( rel2, "\n" );
a4 := f2/rel2;
Print( a4, "\n" );
ga4 := GeneratorsOfGroup(a4);
Print( ga4, "\n" );
SetName(a4,"a4");
b1 := ga4[1];  b2:=ga4[2];
Print( b1, "\n", b2, "\n" );
H2 := Subgroup(a4,[b1]);
Print( H2, "\n" );
iso := GroupHomomorphismByImages(H1,H2,[a1],[b1]);
Print( iso, "\n" );
inv := InverseGeneralMapping(iso);
Print( inv, "\n" );
fpa := FreeProductWithAmalgamation( s3, a4, iso );
Print( fpa, "\n" );
Print( RelatorsOfFpGroup( fpa ), "\n" );
gg1 := GraphOfGroupsRewritingSystem( fpa );
Print( "gg1 = ", gg1, "\n" );
Print( LeftTransversalsOfGraphOfGroups( gg1 ), "\n" );
gfpa := GeneratorsOfGroup( fpa );
Print( "gfpa = ", gfpa, "\n" );
w2 := (gfpa[1]*gfpa[2]*gfpa[3]^gfpa[4])^3;
Print( "w2 = ", w2, "\n" );
n2 := NormalFormGGRWS( fpa, w2 );
Print( "n2 = ", n2, "\n\n" );

############### now for HNN #################

H3 := Subgroup(a4,[b2]);
Print( H3, "\n" );
i23 := GroupHomomorphismByImages( H2, H3, [b1], [b2] );
Print( "i23 = ", i23, "\n" );
hnn := HnnExtension( a4, i23 );
Print( "hnn = ", hnn, "\n" );
phnn := PresentationFpGroup( hnn );;
TzPrint( phnn );
gg2 := GraphOfGroupsRewritingSystem( hnn );
Print( "gg2 = ", gg2, "\n" );
Print( LeftTransversalsOfGraphOfGroups( gg2 ), "\n" );
gh := GeneratorsOfGroup( hnn );;
w3 := (gh[1]^gh[2])*gh[3]^-1*(gh[1]*gh[3]*gh[2]^2)^2*gh[3]*gh[2];
Print( "w3 = ", w3, "\n" );
n3 := NormalFormGGRWS( hnn, w3 );
Print( "n3 = ", n3, "\n\n");

##################### Graphs of groupoids and their words ############### 

Gfa := SinglePieceGroupoid( fa, [-2,-1] );
ofa := One( fa );
SetName( Gfa, "Gfa" );
Uha := Subgroupoid( Gfa, [ [ ha, [-2,-1] ] ] );
SetName( Uha, "Uha" );
Gfb := SinglePieceGroupoid( fb, [-4,-3] );
ofb := One( fb );
SetName( Gfb, "Gfb" );
Uhb := Subgroupoid( Gfb, [ [ hb, [-4,-3] ] ] );
SetName( Uhb, "Uhb" );
mora := GroupoidHomomorphismFromSinglePiece( 
               Uha, Uhb, homab, [-4,-3], [ofb,ofb] ); 
Print( "mora = ", mora, "\n" ); 
morb := GroupoidHomomorphismFromSinglePiece( 
               Uhb, Uha, homba, [-2,-1], [ofa,ofa] );
Print( "morb = ", morb, "\n" ); 
SetInverseGeneralMapping( mora, morb );
SetInverseGeneralMapping( morb, mora );
gg3 := GraphOfGroupoids( D1, [Gfa,Gfb], [Uha,Uhb], [mora,morb] );
Display( gg3 );
f1 := Arrow( Gfa, a^7, -1, -2);
f2 := Arrow( Gfb, b^-6, -4, -4 );
f3 := Arrow( Gfa, a^-11, -2, -1 );
f4 := Arrow( Gfb, b^9, -3, -4 );
f5 := Arrow( Gfa, a^7, -2, -1 ); 
Print( "[f1,f2,f3,f4,f5] = ", [f1,f2,f3,f4,f5], "\n" ); 
L3 := [ f1, 1, f2, 2, f3, 1, f4, 2, f5 ];
Print( "L3 = ", L3, "\n" ); 
gw3 := GraphOfGroupoidsWord( gg3, 5, L3);
Print( "gw3 = ", gw3, "\n" ); 
nw3 := ReducedGraphOfGroupoidsWord( gw3 );
Print( "nw3 = ", nw3, "\n" ); 
