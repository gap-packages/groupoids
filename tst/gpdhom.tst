##############################################################################
##
#W  gpdhom.tst              groupoids Package                    Chris Wensley
##
#Y  Copyright (C) 2000-2017, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
gap> START_TEST( "groupoids package: gpdhom.tst" );
gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

## make mwohom.tst independent of mwo.tst 
gap> s4 := Group( (1,2,3,4), (3,4) );; 
gap> d8 := Subgroup( s4, [ (1,2,3,4), (1,3) ] );;
gap> SetName( s4, "s4" );  SetName( d8, "d8" ); 
gap> Gs4 := SinglePieceGroupoid( s4, [-15 .. -11] );; 
gap> Gd8 := Groupoid( d8, [-9,-8,-7] );;
gap> c6 := Group( (5,6,7)(8,9) );;
gap> SetName( c6, "c6" );
gap> Gc6 := DomainWithSingleObject( c6, -6 );;
gap> SetName( Gs4, "Gs4" );  SetName( Gd8, "Gd8" );  SetName( Gc6, "Gc6" );  
gap> f2 := FreeGroup( 2 );;
gap> Gf2 := Groupoid( f2, -22 );;
gap> SetName( f2, "f2" );  SetName( Gf2, "Gf2" ); 
gap> q8 := SmallGroup( 8, 4 );;
gap> Gq8 := Groupoid( q8, [ -28, -27 ] );;
gap> SetName( q8, "q8" );  SetName( Gq8, "Gq8" );
gap> gl43 := SpecialLinearGroup( 4, 3 );;
gap> Ggl43 := SinglePieceGroupoid( gl43, [ -35..-31 ] );;
gap> SetName( gl43, "gl43" );  SetName( Ggl43, "Ggl43" );
gap> U3 := UnionOfPieces( [ Gc6, Gd8, Gs4 ] );;
gap> Hs4 := FullSubgroupoid( Gs4, [-14,-13,-12] );; 
gap> SetName( Hs4, "Hs4" ); 
gap> Hd8b := SubgroupoidWithRays( Hs4, d8, [(),(1,2,3),(1,2,4)] );;
gap> SetName( Hd8b, "Hd8b" );


###  Subsection 5.1.1
gap> gen1 := GeneratorsOfGroupoid( Gq8 ); 
[ [f1 : -28 -> -28], [f2 : -28 -> -28], [f3 : -28 -> -28], 
  [<identity> of ... : -28 -> -27] ]
gap> gen2 := GeneratorsOfGroupoid( Hd8b ); 
[ [(1,2,3,4) : -14 -> -14], [(1,3) : -14 -> -14], [(1,2,3) : -14 -> -13], 
  [(1,2,4) : -14 -> -12] ]
gap> images := [ gen2[1]^2, gen2[1]*gen2[2], IdentityArrow(Hd8b,-14), gen2[4] ];
[ [(1,3)(2,4) : -14 -> -14], [(1,2)(3,4) : -14 -> -14], [() : -14 -> -14], 
  [(1,2,4) : -14 -> -12] ]
gap> mor1 := GroupoidHomomorphism( Gq8, Hd8b, gen1, images );
groupoid homomorphism : Gq8 -> Hd8b
[ [ [f1 : -28 -> -28], [f2 : -28 -> -28], [f3 : -28 -> -28], 
      [<identity> of ... : -28 -> -27] ], 
  [ [(1,3)(2,4) : -14 -> -14], [(1,2)(3,4) : -14 -> -14], [() : -14 -> -14], 
      [(1,2,4) : -14 -> -12] ] ]
gap> genq8 := GeneratorsOfGroup( q8 );;
gap> imh := [ (1,3)(2,4), (1,2)(3,4), () ];;
gap> h := GroupHomomorphismByImages( q8, d8, genq8, imh );                     
[ f1, f2, f3 ] -> [ (1,3)(2,4), (1,2)(3,4), () ]
gap> mor2 := GroupoidHomomorphism( Gq8, Hd8b, h, [-14,-12], [(),(1,2,4)] );
groupoid homomorphism : Gq8 -> Hd8b
[ [ [f1 : -28 -> -28], [f2 : -28 -> -28], [f3 : -28 -> -28], 
      [<identity> of ... : -28 -> -27] ], 
  [ [(1,3)(2,4) : -14 -> -14], [(1,2)(3,4) : -14 -> -14], [() : -14 -> -14], 
      [(1,2,4) : -14 -> -12] ] ]
gap> mor1=mor2;
true
gap> e := Arrow( Gq8, Product(genq8), -27, -28 );
[f1*f2*f3 : -27 -> -28]
gap> ImageElm( mor2, e );
[(2,4,3) : -12 -> -14]

## SubSection 5.2.1
gap> IsInjectiveOnObjects( mor2 ); 
true
gap> IsSurjectiveOnObjects( mor2 );
false
gap> ad8 := GroupHomomorphismByImages( d8, d8, 
>               [ (1,2,3,4), (1,3) ], [ (1,4,3,2), (2,4) ] );; 
gap> md8 := GroupoidHomomorphism( Gd8, Gd8, ad8, [-7,-9,-8], [(),(1,3),(2,4)] );
groupoid homomorphism : Gd8 -> Gd8
[ [ [(1,2,3,4) : -9 -> -9], [(1,3) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,4,3,2) : -7 -> -7], [(2,4) : -7 -> -7], [(1,3) : -7 -> -9], 
      [(2,4) : -7 -> -8] ] ]
gap> IsBijectiveOnObjects( md8 );
true
gap> IsEndomorphismWithObjects( md8 );
true
gap> IsAutomorphismWithObjects( md8 );
true

## SubSection 5.2.5
gap> RootGroupHomomorphism( mor2 );
[ f1, f2, f3 ] -> [ (1,3)(2,4), (1,2)(3,4), () ]
gap> ImagesOfObjects( mor2 );      
[ -14, -12 ]
gap> ImageElementsOfRays( mor2 );
[ (), (1,2,4) ]

## SubSection 5.2.6
gap> ObjectGroupHomomorphism( mor1, -27 );
[ f1, f2, f3 ] -> [ (1,4)(2,3), (1,3)(2,4), () ]

## SubSection 5.3.1
gap> inc := InclusionMappingGroupoids( Hs4, Hd8b );
groupoid homomorphism : Hd8b -> Hs4
[ [ [(1,2,3,4) : -14 -> -14], [(1,3) : -14 -> -14], [(1,2,3) : -14 -> -13], 
      [(1,2,4) : -14 -> -12] ], 
  [ [(1,2,3,4) : -14 -> -14], [(1,3) : -14 -> -14], [(1,2,3) : -14 -> -13], 
      [(1,2,4) : -14 -> -12] ] ]

## SubSection 5.3.2
gap> max := MaximalDiscreteSubgroupoid( Hd8b );;
gap> res := RestrictedMappingGroupoids( inc, max );
groupoid homomorphism from several pieces : 
groupoid homomorphism : 
[ [ [(1,2,3,4) : -14 -> -14], [(1,3) : -14 -> -14] ], 
  [ [(1,2,3,4) : -14 -> -14], [(1,3) : -14 -> -14] ] ]
groupoid homomorphism : 
[ [ [(1,4,2,3) : -13 -> -13], [(1,2) : -13 -> -13] ], 
  [ [(1,4,2,3) : -13 -> -13], [(1,2) : -13 -> -13] ] ]
groupoid homomorphism : 
[ [ [(1,2,4,3) : -12 -> -12], [(2,3) : -12 -> -12] ], 
  [ [(1,2,4,3) : -12 -> -12], [(2,3) : -12 -> -12] ] ]

## SubSection 5.3.3
gap> iso1 := IsomorphismNewObjects( Hs4, [-30,-20,-10] ); 
groupoid homomorphism : 
[ [ [(1,2,3,4) : -14 -> -14], [(3,4) : -14 -> -14], [() : -14 -> -13], 
      [() : -14 -> -12] ], 
  [ [(1,2,3,4) : -30 -> -30], [(3,4) : -30 -> -30], [() : -30 -> -20], 
      [() : -30 -> -10] ] ]
gap> inc2 := mor2*inc*iso1;
groupoid homomorphism : 
[ [ [f1 : -28 -> -28], [f2 : -28 -> -28], [f3 : -28 -> -28], 
      [<identity> of ... : -28 -> -27] ], 
  [ [(1,3)(2,4) : -30 -> -30], [(1,2)(3,4) : -30 -> -30], [() : -30 -> -30], 
      [(1,2,4) : -30 -> -10] ] ]
gap> iso2 := IsomorphismStandardGroupoid( Hd8b, [-23,-22,-21] );
groupoid homomorphism : 
[ [ [(1,2,3,4) : -14 -> -14], [(1,3) : -14 -> -14], [(1,2,3) : -14 -> -13], 
      [(1,2,4) : -14 -> -12] ], 
  [ [(1,2,3,4) : -23 -> -23], [(1,3) : -23 -> -23], [() : -23 -> -22], 
      [() : -23 -> -21] ] ]
gap> inv2 := InverseGeneralMapping( iso2 );
groupoid homomorphism : 
[ [ [(1,2,3,4) : -23 -> -23], [(1,3) : -23 -> -23], [() : -23 -> -22], 
      [() : -23 -> -21] ], 
  [ [(1,2,3,4) : -14 -> -14], [(1,3) : -14 -> -14], [(1,2,3) : -14 -> -13], 
      [(1,2,4) : -14 -> -12] ] ]

## SubSection 5.3.4
gap> N2 := Subgroup( q8, [ q8.2] );; 
gap> SetName( N2, "N2" );
gap> Hq8 := SubgroupoidWithRays( Gq8, N2, [ One(q8), q8.1 ] ); 
single piece groupoid with rays: < N2, [ -28, -27 ], [ <identity> of ..., f1 ] >
gap> SetName( Hq8, "Hq8" );
gap> isoHq8 := IsomorphismPermGroupoid( Hq8 );;
gap> MappingToSinglePieceData( isoHq8 );       
[ [ [ f2 ] -> [ (1,3,4,7)(2,5,6,8) ], [ -28, -27 ], 
      [ (), (1,2,4,6)(3,8,7,5) ] ] ]

## SubSection 5.4.1
gap> gend12 := [ (15,16,17,18,19,20), (15,20)(16,19)(17,18) ];; 
gap> d12 := Group( gend12 );; 
gap> Gd12 := Groupoid( d12, [-37,-36,-35,-34] );;
gap> SetName( d12, "d12" );  
gap> SetName( Gd12, "Gd12" );
gap> s3 := Subgroup( d12, [ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ] );;
gap> Gs3 := SubgroupoidByPieces( Gd12, [ [ s3, [-36,-35,-34] ] ] );;
gap> SetName( s3, "s3" );  
gap> SetName( Gs3, "Gs3" );
gap> gend8 := GeneratorsOfGroup( d8 );;
gap> imhd8 := [ ( ), (15,20)(16,19)(17,18) ];;
gap> hd8 := GroupHomomorphismByImages( d8, s3, gend8, imhd8 );;
gap> homd8 := GroupoidHomomorphism( Gd8, Gs3, hd8 ); 
groupoid homomorphism : Gd8 -> Gs3
[ [ [(1,2,3,4) : -9 -> -9], [(1,3) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [() : -36 -> -36], [(15,20)(16,19)(17,18) : -36 -> -36], 
      [() : -36 -> -35], [() : -36 -> -34] ] ]
gap> hc6 := GroupHomomorphismByImages( c6, s3, 
>            [(5,6,7)(8,9)], [(15,16)(17,20)(18,19)] );;
gap> Fs3 := FullSubgroupoid( Gs3, [ -35 ] );; 
gap> SetName( Fs3, "Fs3" ); 
gap> homc6 := GroupoidHomomorphism( Gc6, Fs3, hc6 );;
gap> incFs3 := InclusionMappingGroupoids( Gs3, Fs3 );; 
gap> ihomc6 := homc6 * incFs3; 
groupoid homomorphism : Gc6 -> Gs3
[ [ [(5,6,7)(8,9) : -6 -> -6] ], [ [(15,16)(17,20)(18,19) : -35 -> -35] ] ]
gap> idGs3 := IdentityMapping( Gs3 );;
gap> V3 := ReplaceOnePieceInUnion( U3, 1, Gs3 ); 
groupoid with 3 pieces:
[ Gs3, Gd8, Gc6 ]
gap> homs3 := [ idGs3, homd8, ihomc6 ];; 
gap> homV3 := HomomorphismToSinglePiece( V3, Gs3, homs3 );; 
gap> Display( homV3 );         
homomorphism to single piece groupoid with mappings:
(1) :  groupoid mapping: [ Gs3 ] -> [ Gs3 ]
root homomorphism: [ [ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ], 
  [ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ] ]
images of objects: [ -36, -35, -34 ]
   images of rays: [ [() : -36 -> -36], [() : -36 -> -35], [() : -36 -> -34] ]
(2) :  groupoid mapping: [ Gd8 ] -> [ Gs3 ]
root homomorphism: [ [ (1,2,3,4), (1,3) ], [ (), (15,20)(16,19)(17,18) ] ]
images of objects: [ -36, -35, -34 ]
   images of rays: [ [() : -36 -> -36], [() : -36 -> -35], [() : -36 -> -34] ]
(3) :  groupoid mapping: [ Gc6 ] -> [ Gs3 ]
root homomorphism: [ [ (5,6,7)(8,9) ], [ (15,16)(17,20)(18,19) ] ]
images of objects: [ -35 ]
   images of rays: [ [() : -35 -> -35] ]

## SubSection 5.4.2, Homomorphisms from homogeneous discrete 
gap> Dd8 := MaximalDiscreteSubgroupoid( Gd8 );
homogeneous, discrete groupoid: < d8, [ -9, -8, -7 ] >
gap> id8 := IdentityMapping( d8 );; 
gap> GroupoidHomomorphismFromHomogeneousDiscrete( Dd8, Gd8, [id8,id8,id8], 
>    [-8,-7,-9] );                                                        
groupoid homomorphism : morphism from a homogeneous discrete groupoid:
[ -9, -8, -7 ] -> [ -8, -7, -9 ]
object homomorphisms:
IdentityMapping( d8 )
IdentityMapping( d8 )
IdentityMapping( d8 )

## Section 5.5, Homomorphisms with more than one piece 

## SubSection 5.5.1
gap> isoq8 := IsomorphismNewObjects( Gq8, [-38,-37] ); 
groupoid homomorphism : 
[ [ [f1 : -28 -> -28], [f2 : -28 -> -28], [f3 : -28 -> -28], 
      [<identity> of ... : -28 -> -27] ], 
  [ [f1 : -38 -> -38], [f2 : -38 -> -38], [f3 : -38 -> -38], 
      [<identity> of ... : -38 -> -37] ] ]
gap> Gq8b := Range( isoq8 );; 
gap> SetName( Gq8b, "Gq8b" ); 
gap> V4 := UnionOfPieces( [ V3, Gq8 ] ); 
groupoid with 4 pieces:
[ Gs3, Gq8, Gd8, Gc6 ]
gap> SetName( V4, "V4" ); 
gap> Vs3q8b := UnionOfPieces( [ Gs3, Gq8b ] );; 
gap> SetName( Vs3q8b, "Vs3q8b" ); 
gap> hom4 := HomomorphismByUnion( V4, Vs3q8b, [ homV3, isoq8 ] );; 
gap> Display( hom4 );
magma homomorphism: V4 -> Vs3q8b with pieces :
[ Pcgs([ f1, f2, f3 ]) -> [ f1, f2, f3 ], [ -38, -37 ], 
  [ <identity> of ..., <identity> of ... ] ]
(1) :  groupoid mapping: [ Gs3 ] -> [ Gs3 ]
root homomorphism: [ [ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ], 
  [ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ] ]
images of objects: [ -36, -35, -34 ]
   images of rays: [ [() : -36 -> -36], [() : -36 -> -35], [() : -36 -> -34] ]
(2) :  groupoid mapping: [ Gd8 ] -> [ Gs3 ]
root homomorphism: [ [ (1,2,3,4), (1,3) ], [ (), (15,20)(16,19)(17,18) ] ]
images of objects: [ -36, -35, -34 ]
   images of rays: [ [() : -36 -> -36], [() : -36 -> -35], [() : -36 -> -34] ]
(3) :  groupoid mapping: [ Gc6 ] -> [ Gs3 ]
root homomorphism: [ [ (5,6,7)(8,9) ], [ (15,16)(17,20)(18,19) ] ]
images of objects: [ -35 ]
   images of rays: [ [() : -35 -> -35] ]

## Section 5.6, Groupoid automoprphisms 

## SubSection 5.6.1
gap> a4 := Subgroup( s4, [(1,2,3),(2,3,4)] );; 
gap> SetName( a4, "a4" ); 
gap> gensa4 := GeneratorsOfGroup( a4 );; 
gap> Ga4 := SubgroupoidByPieces( Gs4, [ [a4, [-15,-13,-11]] ] ); 
single piece groupoid: < a4, [ -15, -13, -11 ] >
gap> SetName( Ga4, "Ga4" ); 
gap> d := Arrow( Ga4, (1,3,4), -11, -13 ); 
[(1,3,4) : -11 -> -13]
gap> aut1 := GroupoidAutomorphismByObjectPerm( Ga4, [-13,-11,-15] );; 
gap> Display( aut1 ); 
 groupoid mapping: [ Ga4 ] -> [ Ga4 ]
root homomorphism: [ [ (1,2,3), (2,3,4) ], [ (1,2,3), (2,3,4) ] ]
images of objects: [ -13, -11, -15 ]
   images of rays: [ [() : -13 -> -13], [() : -13 -> -11], [() : -13 -> -15] ]
gap> d1 := ImageElm( aut1, d ); 
[(1,3,4) : -15 -> -11]
gap> h2 := GroupHomomorphismByImages( a4, a4, gensa4, [(2,3,4), (1,3,4)] );; 
gap> aut2 := GroupoidAutomorphismByGroupAuto( Ga4, h2 );; 
gap> Display( aut2 ); 
 groupoid mapping: [ Ga4 ] -> [ Ga4 ]
root homomorphism: [ [ (1,2,3), (2,3,4) ], [ (2,3,4), (1,3,4) ] ]
images of objects: [ -15, -13, -11 ]
   images of rays: [ [() : -15 -> -15], [() : -15 -> -13], [() : -15 -> -11] ]
gap> d2 := ImageElm( aut2, d1 );
[(1,2,4) : -15 -> -11]
gap> im3 := [(), (1,3,2), (2,4,3)];; 
gap> aut3 := GroupoidAutomorphismByRayShifts( Ga4, im3 );; 
gap> Display( aut3 ); 
 groupoid mapping: [ Ga4 ] -> [ Ga4 ]
root homomorphism: [ [ (1,2,3), (2,3,4) ], [ (1,2,3), (2,3,4) ] ]
images of objects: [ -15, -13, -11 ]
   images of rays: [ [() : -15 -> -15], [(1,3,2) : -15 -> -13], 
  [(2,4,3) : -15 -> -11] ]
gap> d3 := ImageElm( aut3, d2 );
[(1,4)(2,3) : -15 -> -11]
gap> d0 := Arrow( Ga4, (2,3,4), -11, -13 );; 
gap> aut4 := GroupoidInnerAutomorphism( Ga4, d0 );;
gap> Display( aut4 );
 groupoid mapping: [ Ga4 ] -> [ Ga4 ]
root homomorphism: [ [ (1,2,3), (2,3,4) ], [ (1,2,3), (2,3,4) ] ]
images of objects: [ -15, -11, -13 ]
   images of rays: [ [() : -15 -> -15], [(2,4,3) : -15 -> -11], 
  [(2,3,4) : -15 -> -13] ]
gap> d4 := ImageElm( aut4, d3 );
[(1,2,4) : -15 -> -13]
gap> aut1234 := aut1*aut2*aut3*aut4;; 
gap> Display( aut1234 ); 
 groupoid mapping: [ Ga4 ] -> [ Ga4 ]
root homomorphism: [ [ (1,2,3), (2,3,4) ], [ (1,4,3), (2,4,3) ] ]
images of objects: [ -11, -13, -15 ]
   images of rays: [ [() : -11 -> -11], [(1,2)(3,4) : -11 -> -13], 
  [(1,2)(3,4) : -11 -> -15] ]
gap> d4  = ImageElm( aut1234, d );
true
gap> inv1234 := InverseGeneralMapping( aut1234 );; 
gap> Display( inv1234 ); 
 groupoid mapping: [ Ga4 ] -> [ Ga4 ]
root homomorphism: [ [ (1,2,3), (2,3,4) ], [ (1,2,4), (1,2,3) ] ]
images of objects: [ -11, -13, -15 ]
   images of rays: [ [() : -11 -> -11], [() : -11 -> -13], 
  [(1,4)(2,3) : -11 -> -15] ]

# SubSection 5.6.3 
gap> AGa4 := AutomorphismGroupOfGroupoid( Ga4 ); 
<group with 8 generators>
gap> AGgens := GeneratorsOfGroup( AGa4);; 
gap> NGa4 := NiceObject( AGa4 );; 
gap> MGa4 := NiceMonomorphism( AGa4 );; 
gap> Size( AGa4 ); 
20736
gap> SetName( AGa4, "AGa4" ); 
gap> SetName( NGa4, "NGa4" ); 
gap> ##  cannot test images of AGgens because of random variations 
gap> ##  Now do some tests!
gap> mgi := MappingGeneratorsImages( MGa4 );; 
gap> autgen := mgi[1];; 
gap> pcgen := mgi[2];;
gap> ngen := Length( autgen );; 
gap> ForAll( [1..ngen], i -> Order(autgen[i]) = Order(pcgen[i]) ); 
true

## SubSection 5.6.4
gap> AGa40 := Groupoid( AGa4, [0] );
single piece groupoid: < AGa4, [ 0 ] >
gap> conj := function(a) 
>            return ArrowNC( true, GroupoidInnerAutomorphism(Ga4,a), 0, 0 ); 
>            end;; 
gap> inner := MappingWithObjectsByFunction( Ga4, AGa40, conj, [0,0,0] );;
gap> a1 := Arrow( Ga4, (1,2,3), -15, -13 );;
gap> inn1 := ImageElm( inner, a1 );;                        
gap> a2 := Arrow( Ga4, (2,3,4), -13, -11 );;
gap> inn2 := ImageElm( inner, a2 );;       
gap> a3 := a1*a2;                      
[(1,3)(2,4) : -15 -> -11]
gap> inn3 := ImageElm( inner, a3 );  
[groupoid homomorphism : Ga4 -> Ga4
[ [ [(1,2,3) : -15 -> -15], [(2,3,4) : -15 -> -15], [() : -15 -> -13], 
      [() : -15 -> -11] ], 
  [ [(1,3,4) : -11 -> -11], [(1,2,4) : -11 -> -11], [(1,3)(2,4) : -11 -> -13],
      [() : -11 -> -15] ] ] : 0 -> 0]
gap> (inn3 = inn1*inn2*inn1) and (inn3 = inn2*inn1*inn2);
true

## SubSection 5.6.5
gap> Hs3 := HomogeneousDiscreteGroupoid( s3, [ -13..-10] ); 
homogeneous, discrete groupoid: < s3, [ -13 .. -10 ] >
gap> aut4 := GroupoidAutomorphismByObjectPerm( Hs3, [-12,-10,-11,-13] ); 
groupoid homomorphism : morphism from a homogeneous discrete groupoid:
[ -13, -12, -11, -10 ] -> [ -12, -10, -11, -13 ]
object homomorphisms:
IdentityMapping( s3 )
IdentityMapping( s3 )
IdentityMapping( s3 )
IdentityMapping( s3 )

gap> gens3 := GeneratorsOfGroup( s3 );; 
gap> g1 := gens3[1];; 
gap> g2 := gens3[2];; 
gap> b1 := GroupHomomorphismByImages( s3, s3, gens3, [g1, g2^g1 ] );; 
gap> b2 := GroupHomomorphismByImages( s3, s3, gens3, [g1^g2, g2 ] );; 
gap> b3 := GroupHomomorphismByImages( s3, s3, gens3, [g1^g2, g2^(g1*g2) ] );; 
gap> b4 := GroupHomomorphismByImages( s3, s3, gens3, [g1^(g2*g1), g2^g1 ] );; 
gap> aut5 := GroupoidAutomorphismByGroupAutos( Hs3, [b1,b2,b3,b4] ); 
groupoid homomorphism : morphism from a homogeneous discrete groupoid:
[ -13, -12, -11, -10 ] -> [ -13, -12, -11, -10 ]
object homomorphisms:
GroupHomomorphismByImages( s3, s3, 
[ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ], 
[ (15,17,19)(16,18,20), (15,18)(16,17)(19,20) ] )
GroupHomomorphismByImages( s3, s3, 
[ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ], 
[ (15,19,17)(16,20,18), (15,20)(16,19)(17,18) ] )
GroupHomomorphismByImages( s3, s3, 
[ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ], 
[ (15,19,17)(16,20,18), (15,16)(17,20)(18,19) ] )
GroupHomomorphismByImages( s3, s3, 
[ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ], 
[ (15,19,17)(16,20,18), (15,18)(16,17)(19,20) ] )

gap> AHs3 := AutomorphismGroupOfGroupoid( Hs3 ); 
<group with 4 generators>
gap> Size( AHs3 );
31104
gap> genAHs3 := GeneratorsOfGroup( AHs3 ); 
[ groupoid homomorphism : morphism from a homogeneous discrete groupoid:
    [ -13, -12, -11, -10 ] -> [ -13, -12, -11, -10 ]
    object homomorphisms:
    ConjugatorAutomorphism( s3, (15,19,17)(16,20,18) )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    , groupoid homomorphism : morphism from a homogeneous discrete groupoid:
    [ -13, -12, -11, -10 ] -> [ -13, -12, -11, -10 ]
    object homomorphisms:
    ConjugatorAutomorphism( s3, (15,20)(16,19)(17,18) )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    , groupoid homomorphism : morphism from a homogeneous discrete groupoid:
    [ -13, -12, -11, -10 ] -> [ -12, -11, -10, -13 ]
    object homomorphisms:
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    , groupoid homomorphism : morphism from a homogeneous discrete groupoid:
    [ -13, -12, -11, -10 ] -> [ -12, -13, -11, -10 ]
    object homomorphisms:
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
     ]
gap> nobAHs3 := NiceObject( AHs3 );; 
gap> nmonAHs3 := NiceMonomorphism( AHs3 );;
gap> w := genAHs3[1];; 
gap> w1 := ImageElm( nmonAHs3, w );; 
gap> x := genAHs3[2];; 
gap> x1 := ImageElm( nmonAHs3, x );; 
gap> y := genAHs3[3];; 
gap> y1 := ImageElm( nmonAHs3, y );; 
gap> z := genAHs3[4];; 
gap> z1 := ImageElm( nmonAHs3, z );; 
gap> u := z*w*y*x*z; 
groupoid homomorphism : morphism from a homogeneous discrete groupoid:
[ -13, -12, -11, -10 ] -> [ -11, -13, -10, -12 ]
object homomorphisms:
IdentityMapping( s3 )
ConjugatorAutomorphism( s3, (15,19,17)(16,20,18) )
IdentityMapping( s3 )
ConjugatorAutomorphism( s3, (15,20)(16,19)(17,18) )

gap> u1 := z1*w1*y1*x1*z1; 
(1,2,4,3)(5,17,23,16,8,20,26,13)(6,18,24,15,7,19,25,14)(9,21,27,12,10,22,28,
11)
gap> imu := ImageElm( nmonAHs3, u );; 
gap> u1 = imu;
true

## Section 5.7 
gap> reps := IrreducibleRepresentations( s4 );;
gap> rep4 := reps[4];; 
gap> Rs4 := Groupoid( Image( rep4 ), Gs4!.objects ); 
single piece groupoid: < Group([ [ [ 0, 1, 0 ], [ 1, 0, 0 ], [ 0, 0, 1 ] ], 
  [ [ 0, 0, 1 ], [ 1, 0, 0 ], [ 0, 1, 0 ] ], 
  [ [ -1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, -1 ] ], 
  [ [ 1, 0, 0 ], [ 0, -1, 0 ], [ 0, 0, -1 ] ] ]), [ -15, -14, -13, -12, -11 
 ] >
gap> gens := GeneratorsOfGroupoid( Gs4 );
[ [(1,2,3,4) : -15 -> -15], [(3,4) : -15 -> -15], [() : -15 -> -14], 
  [() : -15 -> -13], [() : -15 -> -12], [() : -15 -> -11] ]
gap> images := List( gens, 
>        g -> Arrow( Rs4, ImageElm(rep4,g![1]), g![2], g![3] ) ); 
[ [[ [ -1, 0, 0 ], [ 0, 0, 1 ], [ 0, -1, 0 ] ] : -15 -> -15], 
  [[ [ 0, 1, 0 ], [ 1, 0, 0 ], [ 0, 0, 1 ] ] : -15 -> -15], 
  [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -14], 
  [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -13], 
  [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -12], 
  [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -11] ]
gap> mor := GroupoidHomomorphismFromSinglePiece( Gs4, Rs4, gens, images );
groupoid homomorphism : 
[ [ [(1,2,3,4) : -15 -> -15], [(3,4) : -15 -> -15], [() : -15 -> -14], 
      [() : -15 -> -13], [() : -15 -> -12], [() : -15 -> -11] ], 
  [ [[ [ -1, 0, 0 ], [ 0, 0, 1 ], [ 0, -1, 0 ] ] : -15 -> -15], 
      [[ [ 0, 1, 0 ], [ 1, 0, 0 ], [ 0, 0, 1 ] ] : -15 -> -15], 
      [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -14], 
      [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -13], 
      [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -12], 
      [[ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] : -15 -> -11] ] ]
gap> IsMatrixGroupoid( Rs4 ); 
true
gap> a := Arrow( Hs4, (1,4,2), -12, -13 );
[(1,4,2) : -12 -> -13]
gap> ImageElm( mor, a );
[[ [ 0, 0, -1 ], [ -1, 0, 0 ], [ 0, 1, 0 ] ] : -12 -> -13]
gap> rmor := RestrictedMappingGroupoids( mor, Hd8b );
groupoid homomorphism : 
[ [ [(1,2,3,4) : -14 -> -14], [(1,3) : -14 -> -14], [(1,2,3) : -14 -> -13], 
      [(1,2,4) : -14 -> -12] ], 
  [ [[ [ -1, 0, 0 ], [ 0, 0, 1 ], [ 0, -1, 0 ] ] : -14 -> -14], 
      [[ [ 1, 0, 0 ], [ 0, 0, -1 ], [ 0, -1, 0 ] ] : -14 -> -14], 
      [[ [ 0, 0, 1 ], [ -1, 0, 0 ], [ 0, -1, 0 ] ] : -14 -> -13], 
      [[ [ 0, -1, 0 ], [ 0, 0, 1 ], [ -1, 0, 0 ] ] : -14 -> -12] ] ]
gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
gap> STOP_TEST( "gpdhom.tst", 10000 );

#############################################################################
##
#E  gpdhom.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
