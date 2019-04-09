##############################################################################
##
#W  gpdhom.tst              groupoids Package                    Chris Wensley
##
#Y  Copyright (C) 2000-2019, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
gap> START_TEST( "groupoids package: gpdhom.tst" );
gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

## make gpdhom.tst independent of gpd.tst 
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
gap> q8 := QuaternionGroup( 8 );;
gap> Gq8 := Groupoid( q8, [ -28, -27 ] );;
gap> SetName( q8, "q8" );  SetName( Gq8, "Gq8" );
gap> gl43 := SpecialLinearGroup( 4, 3 );;
gap> Ggl43 := SinglePieceGroupoid( gl43, [ -35..-31 ] );;
gap> SetName( gl43, "gl43" );  SetName( Ggl43, "Ggl43" );
gap> U3 := UnionOfPieces( [ Gc6, Gd8, Gs4 ] );;
gap> Hs4 := SubgroupoidByObjects( Gs4, [-14,-13,-12] );; 
gap> SetName( Hs4, "Hs4" ); 
gap> Hd8b := SubgroupoidWithRays( Hs4, d8, [(),(1,2,3),(1,2,4)] );;
gap> SetName( Hd8b, "Hd8b" );
gap> s3a := Group( (1,2), (2,3) );; 
gap> ida := IdentityMapping( s3a );; 
gap> isopc := IsomorphismPcGroup( s3a );; 
gap> s3p := Image( isopc );;
gap> isofp := IsomorphismFpGroup( s3a );; 
gap> s3f := Image( isofp );; 
gap> isos2 := [ ida, isopc, isofp ];;
gap> G2 := GroupoidByIsomorphisms( s3a, [-7,-6,-5], isos2 );; 

###  Subsection 5.1.1
gap> gen1 := GeneratorsOfGroupoid( Gq8 ); 
[ [x : -28 -> -28], [y : -28 -> -28], [y2 : -28 -> -28], 
  [<identity> of ... : -28 -> -27] ]
gap> gen2 := GeneratorsOfGroupoid( Hd8b ); 
[ [(1,2,3,4) : -14 -> -14], [(1,3) : -14 -> -14], [(1,2,3) : -14 -> -13], 
  [(1,2,4) : -14 -> -12] ]
gap> images := [ gen2[1]^2, gen2[1]*gen2[2], IdentityArrow(Hd8b,-14), gen2[4] ];
[ [(1,3)(2,4) : -14 -> -14], [(1,2)(3,4) : -14 -> -14], [() : -14 -> -14], 
  [(1,2,4) : -14 -> -12] ]
gap> mor1 := GroupoidHomomorphism( Gq8, Hd8b, gen1, images );
groupoid homomorphism : Gq8 -> Hd8b
[ [ [x : -28 -> -28], [y : -28 -> -28], [y2 : -28 -> -28], 
      [<identity> of ... : -28 -> -27] ], 
  [ [(1,3)(2,4) : -14 -> -14], [(1,2)(3,4) : -14 -> -14], [() : -14 -> -14], 
      [(1,2,4) : -14 -> -12] ] ]
gap> genq8 := GeneratorsOfGroup( q8 );;
gap> imh := [ (1,3)(2,4), (1,2)(3,4), () ];;
gap> h := GroupHomomorphismByImages( q8, d8, genq8, imh );                     
[ x, y, y2 ] -> [ (1,3)(2,4), (1,2)(3,4), () ]
gap> mor2 := GroupoidHomomorphism( Gq8, Hd8b, h, [-14,-12], [(),(1,2,4)] );
groupoid homomorphism : Gq8 -> Hd8b
[ [ [x : -28 -> -28], [y : -28 -> -28], [y2 : -28 -> -28], 
      [<identity> of ... : -28 -> -27] ], 
  [ [(1,3)(2,4) : -14 -> -14], [(1,2)(3,4) : -14 -> -14], [() : -14 -> -14], 
      [(1,2,4) : -14 -> -12] ] ]
gap> mor1=mor2;
true
gap> e := Arrow( Gq8, Product(genq8), -27, -28 );
[x*y*y2 : -27 -> -28]
gap> ImageElm( mor2, e );
[(2,4,3) : -12 -> -14]

## SubSection 5.2.1
gap> [ IsInjectiveOnObjects( mor2 ), IsSurjectiveOnObjects( mor2 ) ]; 
[ true, false ]
gap> [ IsInjective( mor2 ), IsSurjective( mor2 ) ];
[ false, false ]
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
gap> [ IsInjective( md8 ), IsSurjective( md8 ) ];  
[ true, true ]
gap> [ IsEndomorphismWithObjects( md8 ), IsAutomorphismWithObjects( md8 ) ];
[ true, true ]

## SubSection 5.2.5
gap> RootGroupHomomorphism( mor2 );
[ x, y, y2 ] -> [ (1,3)(2,4), (1,2)(3,4), () ]
gap> ImagesOfObjects( mor2 );      
[ -14, -12 ]
gap> ImageElementsOfRays( mor2 );
[ (), (1,2,4) ]

## SubSection 5.2.6
gap> ObjectGroupHomomorphism( mor1, -27 );
[ x, y, y2 ] -> [ (1,4)(2,3), (1,3)(2,4), () ]

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
[ [ [x : -28 -> -28], [y : -28 -> -28], [y2 : -28 -> -28], 
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
gap> G2;
single piece groupoid with rays: < Group( [ (1,2), (2,3) ] ), [ -7, -6, -5 ], 
[ [ (), () ], [ (), <identity> of ... ], [ (), <identity ...> ] ] >
gap> isoG2 := IsomorphismStandardGroupoid( G2, [-17,-16,-15] ); 
groupoid homomorphism : 
[ [ [[ (1,2), (1,2) ] : -7 -> -7], [[ (2,3), (2,3) ] : -7 -> -7], 
      [[ (), <identity> of ... ] : -7 -> -6], [[ (), <identity ...> ] : -7 -> 
        -5] ], 
  [ [(1,2) : -17 -> -17], [(2,3) : -17 -> -17], [() : -17 -> -16], 
      [() : -17 -> -15] ] ]

## SubSection 5.3.4
gap> N2 := Subgroup( q8, [ q8.2] );; 
gap> SetName( N2, "N2" );
gap> Hq8 := SubgroupoidWithRays( Gq8, N2, [ One(q8), q8.1 ] ); 
single piece groupoid with rays: < N2, [ -28, -27 ], [ <identity> of ..., x ] >
gap> SetName( Hq8, "Hq8" );
gap> isoHq8 := IsomorphismPermGroupoid( Hq8 );;
gap> MappingToSinglePieceData( isoHq8 );       
[ [ [ y ] -> [ (1,3,4,7)(2,5,6,8) ], [ -28, -27 ], 
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
gap> Fs3 := SubgroupoidByObjects( Gs3, [ -35 ] );; 
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
[ [ [x : -28 -> -28], [y : -28 -> -28], [y2 : -28 -> -28], 
      [<identity> of ... : -28 -> -27] ], 
  [ [x : -38 -> -38], [y : -38 -> -38], [y2 : -38 -> -38], 
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
[ Pcgs([ x, y, y2 ]) -> [ x, y, y2 ], [ -38, -37 ], 
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
gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
gap> STOP_TEST( "gpdhom.tst", 10000 );
