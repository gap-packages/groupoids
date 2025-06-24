############################################################################
##
#W  gpdhom.tst              groupoids Package                  Chris Wensley
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
gap> Gc6 := MagmaWithSingleObject( c6, -10 );;
gap> SetName( Gs4, "Gs4" );  SetName( Gd8, "Gd8" );  SetName( Gc6, "Gc6" );  
gap> HDc6 := HomogeneousDiscreteGroupoid( c6, [-27..-24] );;
gap> SetName( HDc6, "HDc6" );
gap> f2 := FreeGroup( 2 );;
gap> Gf2 := Groupoid( f2, -22 );;
gap> SetName( f2, "f2" );  SetName( Gf2, "Gf2" ); 
gap> q8 := QuaternionGroup( 8 );;
gap> genq8 := GeneratorsOfGroup( q8 );;
gap> x := genq8[1];;  y := genq8[2];;
gap> Gq8 := Groupoid( q8, [ -18, -17 ] );;
gap> SetName( q8, "q8" );  SetName( Gq8, "Gq8" );
gap> gl43 := SpecialLinearGroup( 4, 3 );;
gap> Ggl43 := SinglePieceGroupoid( gl43, [ -23..-21 ] );;
gap> SetName( gl43, "gl43" );  SetName( Ggl43, "Ggl43" );
gap> U3 := UnionOfPieces( [ Gs4, Gc6, Gd8 ] );;
gap> Kd8 := SubgroupoidWithRays( Gs4, d8, 
>               [ (), (1,2,3), (1,2,4), (1,3,4), (2,3,4) ] );;
gap> SetName( Kd8, "Kd8" );
gap> Hs4 := SubgroupoidByObjects( Gs4, [-14,-13,-12] );; 
gap> SetName( Hs4, "Hs4" ); 
gap> s3a := Group( (1,2), (2,3) );; 
gap> SetName( s3a, "s3a" );
gap> ida := IdentityMapping( s3a );; 
gap> isopc := IsomorphismPcGroup( s3a );; 
gap> s3p := Image( isopc );;
gap> SetName( s3p, "s3p" );
gap> isofp := IsomorphismFpGroup( s3a );; 
gap> s3f := Image( isofp );;
gap> SetName( s3f, "s3f" ); 
gap> isos2 := [ ida, isopc, isofp ];;
gap> G2 := GroupoidByIsomorphisms( s3a, [-6,-5,-4], isos2 );; 

###  Subsection 5.1.1
gap> (1,2,4)^-1 * (1,3,4);
(2,3,4)
gap> Hd8 := SubgroupoidByObjects( Kd8, [ -13, -12 ] );
single piece groupoid with rays: < Group( [ (1,2,4,3), (2,3) ] ), 
[ -13, -12 ], [ (), (2,3,4) ] >
gap> SetName( Hd8, "Hd8" );
gap> gen1 := GeneratorsOfGroupoid( Gq8 ); 
[ [x : -18 -> -18], [y : -18 -> -18], [y2 : -18 -> -18], 
  [<identity> of ... : -18 -> -17] ]
gap> gen2 := GeneratorsOfGroupoid( Hd8 ); 
[ [(1,2,4,3) : -13 -> -13], [(2,3) : -13 -> -13], [(2,3,4) : -13 -> -12] ]
gap> images := [ gen2[1]^2, gen2[1]*gen2[2], gen2[2]^2, gen2[3] ];
[ [(1,4)(2,3) : -13 -> -13], [(1,3)(2,4) : -13 -> -13], [() : -13 -> -13], 
  [(2,3,4) : -13 -> -12] ]
gap> hom8 := GroupoidHomomorphismFromSinglePiece( Gq8, Hd8, gen1, images );
groupoid homomorphism : Gq8 -> Hd8
[ [ [x : -18 -> -18], [y : -18 -> -18], [y2 : -18 -> -18], 
      [<identity> of ... : -18 -> -17] ], 
  [ [(1,4)(2,3) : -13 -> -13], [(1,3)(2,4) : -13 -> -13], [() : -13 -> -13], 
      [(2,3,4) : -13 -> -12] ] ]
gap> genq8 := GeneratorsOfGroup( q8 );;
gap> imh := [ (1,4)(2,3), (1,3)(2,4), () ];;
gap> h := GroupHomomorphismByImages( q8, d8, genq8, imh );                     
[ x, y, y2 ] -> [ (1,4)(2,3), (1,3)(2,4), () ]
gap> hom9 := GroupoidHomomorphism( Gq8, Hd8, h, [-13,-12], [(),(2,3,4)] );
groupoid homomorphism : Gq8 -> Hd8
[ [ [x : -18 -> -18], [y : -18 -> -18], [y2 : -18 -> -18], 
      [<identity> of ... : -18 -> -17] ], 
  [ [(1,4)(2,3) : -13 -> -13], [(1,3)(2,4) : -13 -> -13], [() : -13 -> -13], 
      [(2,3,4) : -13 -> -12] ] ]
gap> hom8 = hom9;
true
gap> e1 := Arrow( Gq8, Product(genq8), -17, -18 );
[x*y*y2 : -17 -> -18]
gap> e2 := ImageElm( hom8, e1 );
[(1,2,3) : -12 -> -13]

## SubSection 5.2.1
gap> [ IsInjectiveOnObjects( hom8 ), IsSurjectiveOnObjects( hom8 ) ]; 
[ true, true ]
gap> [ IsInjective( hom8 ), IsSurjective( hom8 ) ];
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

## SubSection 5.2.3
gap> RootGroupHomomorphism( hom8 );
[ x, y, y2 ] -> [ (1,4)(2,3), (1,3)(2,4), () ]
gap> ImagesOfObjects( hom8 );   
[ -13, -12 ]
gap> ImageElementsOfRays( hom8 );
[ (), (2,3,4) ]

## SubSection 5.2.6
gap> MappingGeneratorsImages( hom8 );
[ [ [x : -18 -> -18], [y : -18 -> -18], [y2 : -18 -> -18], 
      [<identity> of ... : -18 -> -17] ], 
  [ [(1,4)(2,3) : -13 -> -13], [(1,3)(2,4) : -13 -> -13], [() : -13 -> -13], 
      [(2,3,4) : -13 -> -12] ] ]
gap> MappingToSinglePieceData( hom8 );
[ [ [ x, y, y2 ] -> [ (1,4)(2,3), (1,3)(2,4), () ], [ -13, -12 ], 
      [ (), (2,3,4) ] ] ]

## SubSection 5.2.7
gap> ObjectGroupHomomorphism( hom8, -17 );
[ x, y, y2 ] -> [ (1,2)(3,4), (1,4)(2,3), () ]

## SubSection 5.3.1
gap> inc8 := InclusionMappingGroupoids( Kd8, Hd8 );
groupoid homomorphism : Hd8 -> Kd8
[ [ [(1,2,4,3) : -13 -> -13], [(2,3) : -13 -> -13], [(2,3,4) : -13 -> -12] ], 
  [ [(1,2,4,3) : -13 -> -13], [(2,3) : -13 -> -13], [(2,3,4) : -13 -> -12] ] ]

## SubSection 5.3.2
gap> Md8 := MaximalDiscreteSubgroupoid( Hd8 ); 
groupoid with 2 pieces:
1:  single piece groupoid: < Group( [ (1,2,4,3), (2,3) ] ), [ -13 ] >
2:  single piece groupoid: < Group( [ (1,3,2,4), (3,4) ] ), [ -12 ] >
gap> res8 := RestrictedMappingGroupoids( inc8, Md8 );
groupoid homomorphism from several pieces : 
groupoid homomorphism : 
[ [ [(1,2,4,3) : -13 -> -13], [(2,3) : -13 -> -13] ], 
  [ [(1,2,4,3) : -13 -> -13], [(2,3) : -13 -> -13] ] ]
groupoid homomorphism : 
[ [ [(1,3,2,4) : -12 -> -12], [(3,4) : -12 -> -12] ], 
  [ [(1,3,2,4) : -12 -> -12], [(3,4) : -12 -> -12] ] ]
gap> ParentMappingGroupoids( res8 ) = inc8; 
true

## SubSection 5.3.3
gap> iso4 := IsomorphismNewObjects( Hs4, [-30..-28] );
groupoid homomorphism : 
[ [ [(1,2,3,4) : -14 -> -14], [(3,4) : -14 -> -14], [() : -14 -> -13], 
      [() : -14 -> -12] ], 
  [ [(1,2,3,4) : -30 -> -30], [(3,4) : -30 -> -30], [() : -30 -> -29], 
      [() : -30 -> -28] ] ]
gap> Ks4 := Range( iso4 );
single piece groupoid: < s4, [ -30 .. -28 ] >
gap> IsSubgroupoid( Hs4, Hd8 );                                       
true
gap> inc2 := InclusionMappingGroupoids( Hs4, Hd8 );; 
gap> mor8 := hom8 * inc2 * iso4;
groupoid homomorphism : 
[ [ [x : -18 -> -18], [y : -18 -> -18], [y2 : -18 -> -18], 
      [<identity> of ... : -18 -> -17] ], 
  [ [(1,4)(2,3) : -29 -> -29], [(1,3)(2,4) : -29 -> -29], [() : -29 -> -29], 
      [(2,3,4) : -29 -> -28] ] ]
gap> ImageElm( mor8, e1 );
[(1,2,3) : -28 -> -29]

## SubSection 5.3.4
gap> iso8 := IsomorphismStandardGroupoid( Hd8, [-41,-40] ); 
groupoid homomorphism : 
[ [ [(1,2,4,3) : -13 -> -13], [(2,3) : -13 -> -13], [(2,3,4) : -13 -> -12] ], 
  [ [(1,2,4,3) : -41 -> -41], [(2,3) : -41 -> -41], [() : -41 -> -40] ] ]
gap> e3 := ImageElm( iso8, e2 );
[(1,2)(3,4) : -40 -> -41]
gap> inv8 := InverseGeneralMapping( iso8 ); 
groupoid homomorphism : 
[ [ [(1,2,4,3) : -41 -> -41], [(2,3) : -41 -> -41], [() : -41 -> -40] ], 
  [ [(1,2,4,3) : -13 -> -13], [(2,3) : -13 -> -13], [(2,3,4) : -13 -> -12] ] ]
gap> Ld8 := Image( iso8 );   SetName( Ld8, "Ld8" );
single piece groupoid: < Group( [ (1,2,4,3), (2,3) ] ), [ -41, -40 ] >

gap> G2;
single piece groupoid with rays: < s3a, [ -6, -5, -4 ], 
[ [ (), () ], [ (), <identity> of ... ], [ (), <identity ...> ] ] >
gap> isoG2 := IsomorphismStandardGroupoid( G2, [-44,-43,-42] ); 
groupoid homomorphism : 
[ [ [[ (1,2), (1,2) ] : -6 -> -6], [[ (2,3), (2,3) ] : -6 -> -6], 
      [[ (), <identity> of ... ] : -6 -> -5], [[ (), <identity ...> ] : -6 -> 
        -4] ], 
  [ [(1,2) : -44 -> -44], [(2,3) : -44 -> -44], [() : -44 -> -43], 
      [() : -44 -> -42] ] ]

## SubSection 5.3.5
gap> C4 := Subgroup( q8, [ q8.2] );; 
gap> SetName( C4, "C4" );
gap> Hq8 := SubgroupoidWithRays( Gq8, C4, [ One(q8), q8.1 ] ); 
single piece groupoid with rays: < C4, [ -18, -17 ], [ <identity> of ..., x ] >
gap> SetName( Hq8, "Hq8" );
gap> isoHq8 := IsomorphismPermGroupoid( Hq8 );;
gap> regHq8 := RegularActionHomomorphismGroupoid( Hq8 );
groupoid homomorphism : 
[ [ [y : -18 -> -18], [x : -18 -> -17] ], 
  [ [(1,3,4,7)(2,5,6,8) : -18 -> -18], [(1,2,4,6)(3,8,7,5) : -18 -> -17] ] ]

## SubSection 5.4.1

gap> U8 := UnionOfPieces( Gq8, Ld8 );;
gap> mU8 := HomomorphismToSinglePiece( U8, Hd8, [ inv8, hom8 ] );      
groupoid homomorphism : 
[ [ [ [(1,2,4,3) : -41 -> -41], [(2,3) : -41 -> -41], [() : -41 -> -40] ], 
      [ [(1,2,4,3) : -13 -> -13], [(2,3) : -13 -> -13], 
          [(2,3,4) : -13 -> -12] ] ], 
  [ 
      [ [x : -18 -> -18], [y : -18 -> -18], [y2 : -18 -> -18], 
          [<identity> of ... : -18 -> -17] ], 
      [ [(1,4)(2,3) : -13 -> -13], [(1,3)(2,4) : -13 -> -13], 
          [() : -13 -> -13], [(2,3,4) : -13 -> -12] ] ] ]
gap> e2 = ImageElm( mU8, e3 );
true

## SubSection 5.4.2, Homomorphisms from homogeneous discrete 
gap> c3a := Subgroup( s4, [(1,2,3)] );; c3b := Subgroup( s4, [(1,2,4)] );;
gap> c3c := Subgroup( s4, [(1,3,4)] );; c3d := Subgroup( s4, [(2,3,4)] );;
gap> hc6a := GroupHomomorphismByImages( c6, c3a, [(5,6,7)(8,9)], [(1,2,3)] );;
gap> hc6b := GroupHomomorphismByImages( c6, c3b, [(5,6,7)(8,9)], [(1,2,4)] );;
gap> hc6c := GroupHomomorphismByImages( c6, c3c, [(5,6,7)(8,9)], [(1,3,4)] );;
gap> hc6d := GroupHomomorphismByImages( c6, c3d, [(5,6,7)(8,9)], [(2,3,4)] );;
gap> mor6 := GroupoidHomomorphismFromHomogeneousDiscrete( HDc6, Gs4,
>              [ hc6a, hc6b, hc6c, hc6d ], [-15,-14,-12,-11] );
groupoid homomorphism : HDc6 -> Gs4
gap> e6 := Arrow( HDc6, (5,6,7), -25, -25 );;
gap> ImageElm( mor6, e6 );
[(1,3,4) : -12 -> -12]

## Section 5.5, Homomorphisms with more than one piece 

## SubSection 5.5.1
gap> isoq8 := IsomorphismNewObjects( Gq8, [-38,-37] ); 
groupoid homomorphism : 
[ [ [x : -18 -> -18], [y : -18 -> -18], [y2 : -18 -> -18], 
      [<identity> of ... : -18 -> -17] ], 
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
gap> z := Arrow( Gq8, x*y, -17, -18 );;
gap> ImageElm( hom4, z );
[x*y : -37 -> -38]
gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
gap> STOP_TEST( "gpdhom.tst", 10000 );
