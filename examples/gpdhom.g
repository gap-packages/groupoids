##############################################################################
##
#W  gpdhom.g                 GAP4 package `groupoids'            Chris Wensley
#W                                                                & Emma Moore
#Y  Copyright (C) 2000-2017, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

SetInfoLevel( InfoGroupoids, 4 );

Print( "\n============================================================\n");
Print(   "<<<<    testing examples in the groupoids manual        >>>>\n" );
Print(   "<<<< functions for groupoids and their homomorphisms    >>>>\n" );
Print( "==============================================================\n\n");

gend12 := [ (15,16,17,18,19,20), (15,20)(16,19)(17,18) ];; 
d12 := Group( gend12 );; 
Gd12 := SinglePieceGroupoid( d12, [-37 .. -34] );;
SetName( d12, "d12" );  
SetName( Gd12, "Gd12" );
s3 := Subgroup( d12, [ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ] );
Gs3 := SubgroupoidByPieces( Gd12, [ [ s3, [-36,-35,-34] ] ] );;
SetName( s3, "s3" );  
SetName( Gs3, "Gs3" ); 
gend8 := GeneratorsOfGroup( d8 );
imhd8 := [ ( ), (15,20)(16,19)(17,18) ];
hd8 := GroupHomomorphismByImages( d8, s3, gend8, imhd8 );
homd8 := GroupoidHomomorphismFromSinglePiece( Gd8, Gs3, hd8, 
             [-34,-35,-36], [(),(),()] ); 
Print( "[e2, ImageElm( homd8, e2 )] = ", [e2, ImageElm( homd8, e2 )], "\n" );
incGs3 := InclusionMappingGroupoids( Gd12, Gs3 ); 
ihomd8 := homd8 * incGs3; 
Print( "ihomd8 = ", ihomd8, "\n" );
Print( "IsBijectiveOnObjects(ihomd8)? ",IsBijectiveOnObjects(ihomd8),"\n" );
TestAllProductsUnderGroupoidHomomorphism( ihomd8 ); 
Print( "\n\n" ); 
c6d := Subgroup( d12, [ (15,16,17,18,19,20) ] ); 
rays := [ (), (16,20)(17,19), (15,17)(18,20), (15,19)(16,18) ]; 
Gc6d := SubgroupoidWithRays( Gd12, c6d, rays ); 
iso1 := IsomorphismNewObjects( Gd12, [-7,-6,-5,-4] ); 
Print( "iso1 = ", iso1, "\n" ); 
iso2 := IsomorphismStandardGroupoid( Gc6d, [-7,-6,-5,-4] ); 
Print( "iso2 = ", iso2 ); 
Print( "\n\n" ); 

hc6 := GroupHomomorphismByImages( c6, s3, 
          [(5,6,7)(8,9)], [(15,16)(17,20)(18,19)] );;
Fs3 := FullSubgroupoid( Gs3, [ -35 ] ); 
SetName( Fs3, "Fs3" ); 
homc6 := GroupoidHomomorphism( Gc6, Fs3, hc6 );
Print( "homc6 = " );
Display( homc6 ); 
Print( "\n" ); 
incFs3 := InclusionMappingGroupoids( Gs3, Fs3 ); 
Print( "incFs3 = " );
Display( incFs3 ); 
Print( "\n" ); 
ihomc6 := homc6 * incFs3; 
Print( "ihomc6 = " );
Display( ihomc6 ); 

idGs3 := IdentityMapping( Gs3 );
V3 := ReplaceOnePieceInUnion( U3, 1, Gs3 ); 
Print( "V3 = ", V3, "\n" ); 
Print( "\n" ); 
images3 := [ PieceImages( idGs3 )[1], 
             PieceImages( homd8 )[1], 
             PieceImages( ihomc6 )[1] ]; 
homV3 := HomomorphismToSinglePiece( V3, Gs3, images3 ); 
Print( "\ncombined homomorphism to Gs3:\n" ); 
Display( homV3 ); 

isoq8 := IsomorphismNewObjects( Gq8, [-38,-37] ); 
Print( "\nisoq8 = ", isoq8, "\n" ); 
Gq8b := Range( isoq8 ); 
SetName( Gq8b, "Gq8b" ); 
V4 := UnionOfPieces( [ V3, Gq8 ] ); 
SetName( V4, "V4" ); 
Vs3q8b := UnionOfPieces( [ Gs3, Gq8b ] ); 
SetName( Vs3q8b, "Vs3q8b" ); 
hom4 := HomomorphismByUnion( V4, Vs3q8b, [ homV3, isoq8 ] ); 
Print( "\nhomomorphism by union:\n" ); 
Display( hom4 ); 

## testing methods for \= 
genq8 := GeneratorsOfGroup(q8);
genq8a := List( genq8, g -> g^genq8[2] );
autq8 := GroupHomomorphismByImages( q8, q8, genq8, genq8a ); 
autGq8 := GroupoidAutomorphismByGroupAuto( Gq8, autq8 ); 
isoq8b := autGq8*isoq8; 
Print( "isoq8 = isoq8b ? ", isoq8 = isoq8b, "\n" ); 
hom4b := HomomorphismByUnion( V4, Vs3q8b, [ homV3, isoq8b ] ); 
Print( "hom4 = hom4b ? ", hom4 = hom4b, "\n" ); 

Print( "\nNow for examples of automorphisms of single piece groupoids" ); 
Print( "\n===========================================================\n" );
a4 := Subgroup( s4, [(1,2,3),(2,3,4)] ); 
SetName( a4, "a4" ); 
gensa4 := GeneratorsOfGroup( a4 ); 
Ga4 := SubgroupoidByPieces( Gs4, [ [a4, [-15,-13,-11]] ] );
Print( "Ga4 = ", Ga4, "\n" ); 
SetName( Ga4, "Ga4" ); 

aut1 := GroupoidAutomorphismByObjectPerm( Ga4, [-13,-11,-15] ); 
Print( "\nmapping aut1 = \n" ); 
Display( aut1 ); 

h2 := GroupHomomorphismByImages( a4, a4, gensa4, [(2,3,4), (1,3,4)] ); 
aut2 := GroupoidAutomorphismByGroupAuto( Ga4, h2 ); 
Print( "\nmapping aut2 = \n" ); 
Display( aut2 ); 

im3 := [(), (1,3,2), (2,4,3)]; 
aut3 := GroupoidAutomorphismByRayShifts( Ga4, im3 ); 
Print( "\nmapping aut3 = \n" ); 
Display( aut3 ); 

aut123 := aut1*aut2*aut3; 
Print( "\nmapping aut123 = \n" ); 
Display( aut123 ); 
inv123 := InverseGeneralMapping( aut123 ); 
Print( "\nmapping inv123 = \n" ); 
Display( inv123 ); 
id123 := aut123 * inv123; 
Print( "\nid123 = identity mapping? ", id123=IdentityMapping(Ga4), "\n\n" ); 

## attempt to create automorphism group for Ga4
AGa4 := AutomorphismGroupOfGroupoid( Ga4 ); 
AGgens := GeneratorsOfGroup( AGa4); 
Print( "AGa4 has generators:\n", AGgens, "\n" ); 
NGa4 := NiceObject( AGa4 ); 
MGa4 := NiceMonomorphism( AGa4 ); 
Print( "Ga4 has automorphism group AGa4 of size ", Size( AGa4 ) ); 
Print( " with pc-group representation :-\n", NGa4, "\n" ); 
SetName( NGa4, "NGa4" ); 
Print( "NGa4 has nice monomorphism :-\n", MGa4, "\n\n" ); 

Print( "Now do some tests!\n==================\n\n" ); 
mgi := MappingGeneratorsImages( MGa4 ); 
autgen := mgi[1]; 
pcgen := mgi[2];
ngen := Length( autgen ); 
ok := ForAll( [1..ngen], i -> Order(autgen[i]) = Order(pcgen[i]) ); 
Print( "check that MGa4 preserves orders of elements: ", ok, "\n\n" );  


Print( "\n\nNow for homomorphisms of discrete, homogeneous groupoids" ); 
Print( "\n=========================================================\n" );

Print( "Hc6 = ", Hc6, "\n" ); 
hc6b := GroupHomomorphismByImages( c6, s3, 
          [(5,6,7)(8,9)], [(15,18)(16,17)(19,20)] );;
hc6c := GroupHomomorphismByImages( c6, s3, 
          [(5,6,7)(8,9)], [(15,20)(16,19)(17,18)] );;
hc6d := GroupHomomorphismByImages( c6, s3, 
          [(5,6,7)(8,9)], [(15,17,19)(16,18,20)] );;
Print( "[hc6,hc6b,hc6c,hc6d] = ", [hc6,hc6b,hc6c,hc6d], "\n" ); 
hs3 := MappingToOne( s3, c6 ); 
Hs3 := HomogeneousDiscreteGroupoid( s3, [-13..-10] );
Print( "Hs3 = ", Hs3, "\n" ); 
hc6s3 := GroupoidHomomorphismFromHomogeneousDiscrete( 
    Hc6, Hs3, [ hc6, hc6b, hc6c, hc6d ], [-11,-10,-13,-12] ); 
Print( "hc6s3 = ", hc6s3, "\n" ); 
hs3c6 := GroupoidHomomorphismFromHomogeneousDiscrete( 
    Hs3, Hc6, [ hs3, hs3, hs3, hs3 ], [-4,-5,-6,-7] ); 
Print( "hs3c6 = ", hs3c6, "\n" ); 
e6 := Arrow( Hc6, (5,6,7)(8,9), -4, -4 ); 
i6 := ImageElm( hc6s3, e6 ); 
Print( "e6 = ", e6, " -> i6 = ", i6, "\n" ); 
ids3 := IdentityMapping( s3 ); 
hs3s3 := GroupoidHomomorphismFromHomogeneousDiscrete( 
    Hs3, Gs3, [ ids3, ids3, ids3, ids3 ], [ -36, -36, -34, -34 ] ); 
Print( "hs3s3 = ", hs3s3, "\n" ); 
hprod := hs3s3 * incGs3; 
Print( "hprod = ", hprod, "\n" ); 

Print( "\n\nNow for automorphisms of discrete, homogeneous groupoids" ); 
Print( "\n=========================================================\n" );
Hs3 := HomogeneousDiscreteGroupoid( s3, [ -13..-10] ); 
Print( "Hs3 = ", Hs3, "\n" ); 
aut4 := GroupoidAutomorphismByObjectPerm( Hs3, [-12,-10,-11,-13] ); 
Print( "\nautomorphism aut4 of Hs3 :-\n" ); 
Display( aut4 ); 
gens3 := GeneratorsOfGroup( s3 );; 
g1 := gens3[1]; 
g2 := gens3[2]; 
b1 := GroupHomomorphismByImages( s3, s3, gens3, [ g1, g2^g1 ] ); 
b2 := GroupHomomorphismByImages( s3, s3, gens3, [ g1^g2, g2 ] ); 
b3 := GroupHomomorphismByImages( s3, s3, gens3, [ g1^g2, g2^(g1*g2) ] ); 
b4 := GroupHomomorphismByImages( s3, s3, gens3, [ g1^(g2*g1), g2^g1 ] ); 
aut5 := GroupoidAutomorphismByGroupAutos( Hs3, [b1,b2,b3,b4] ); 
Print( "\nautomorphism aut5 of Hs3 :-\n" ); 
Display( aut5 ); 

## attempt to create automorphism group for Hs3 
AHs3 := AutomorphismGroupOfGroupoid( Hs3 ); 
NHs3 := NiceObject( AHs3 ); 
Print( "Hs3 has automorphism group AHs3 of size ", Size( AHs3 ) ); 
Print( " with pc-group representation :-\n", NHs3, "\n" ); 
SetName( NHs3, "NHs3" ); 
MHs3 := NiceMonomorphism( AHs3 ); 
Print( "NHs3 has nice monomorphism :-\n", MHs3, "\n\n" ); 
