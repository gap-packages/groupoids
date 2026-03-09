############################################################################
##
#W  gpdhom.tst              groupoids Package                  Chris Wensley
##

gap> START_TEST( "groupoids package: gpdhom.tst" );
gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

## make gpdhom.tst independent of gpd.tst 
gap> a4 := Group( (1,2,3), (2,3,4) );; 
gap> d8 := Group( (5,6,7,8), (5,7) );;
gap> SetName( a4, "a4" );  SetName( d8, "d8" ); 
gap> Ga4 := SinglePieceGroupoid( a4, [-15 .. -11] );; 
gap> Gd8 := Groupoid( d8, [-9,-8,-7,-6] );;
gap> c6 := Group( (11,12,13)(14,15) );;
gap> SetName( c6, "c6" );
gap> Gc6 := MagmaWithSingleObject( c6, -10 );;
gap> SetName( Ga4, "Ga4" );  SetName( Gd8, "Gd8" );  SetName( Gc6, "Gc6" );  
gap> f2 := FreeGroup( 2 );;
gap> Gf2 := Groupoid( f2, -20 );;
gap> SetName( f2, "f2" );  SetName( Gf2, "Gf2" ); 
gap> q8 := QuaternionGroup( 8 );;
gap> genq8 := GeneratorsOfGroup( q8 );;
gap> x := genq8[1];;  y := genq8[2];;
gap> Gq8 := Groupoid( q8, [ -19, -18, -17 ] );;
gap> SetName( q8, "q8" );  SetName( Gq8, "Gq8" );
gap> sl43 := SpecialLinearGroup( 4, 3 );;
gap> Gsl43 := SinglePieceGroupoid( sl43, [-23,-22,-21] );;
gap> SetName( sl43, "sl43" );  SetName( Gsl43, "Gsl43" );
gap> U3 := UnionOfPieces( [ Ga4, Gc6, Gd8 ] );;
gap> HDc6 := HomogeneousDiscreteGroupoid( c6, [-27..-24] );;
gap> SetName( HDc6, "HDc6" );
gap> k4 := Subgroup( a4, [ (1,2)(3,4), (1,3)(2,4) ] );;
gap> SetName( k4, "k4" );
gap> Gk4 := SubgroupoidWithRays( Ga4, k4, 
>               [ (), (1,2,3), (1,2,4), (1,3,4), (2,3,4) ] );;
gap> c3a := Subgroup( a4, [ (1,2,3) ] );; 
gap> SetName( c3a, "c3a" );
gap> Ha4 := SubgroupoidByObjects( Ga4, [-14,-13,-12,-11] );;  
gap> SetName( Ha4, "Ha4" );
gap> Hc3a := SubgroupoidBySubgroup( Ha4, c3a );;
gap> c3b := Subgroup( a4, [ (1,2,4) ] );;
gap> SetName( c3b, "c3b" );
gap> Jc3 := Subgroupoid( Ha4, [ [ c3a, [-14] ], [ c3b, [-13,-12] ] ] );;
gap> SetName( Jc3, "Jc3" );
gap> c4 := Subgroup( d8, [ (5,6,7,8) ] );;  SetName( c4, "c4" );
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

###  Section 5.1
###  Subsection 5.1.1
gap> Kk4 := SubgroupoidWithRays( Ha4, k4, [ (), (1,2,3), (1,2,4), (1,3,4) ] );;
gap> SetName( Kk4, "Kk4" );
gap> gen1 := GeneratorsOfGroupoid( Gd8 ); 
[ [(5,6,7,8) : -9 -> -9], [(5,7) : -9 -> -9], [() : -9 -> -8], 
  [() : -9 -> -7], [() : -9 -> -6] ]
gap> gen2 := GeneratorsOfGroupoid( Kk4 ); 
[ [(1,2)(3,4) : -14 -> -14], [(1,3)(2,4) : -14 -> -14], 
  [(1,2,3) : -14 -> -13], [(1,2,4) : -14 -> -12], [(1,3,4) : -14 -> -11] ]
gap> images := [ gen2[1]*gen2[2], gen2[1]^2, gen2[3], gen2[4], gen2[5] ];
[ [(1,4)(2,3) : -14 -> -14], [() : -14 -> -14], [(1,2,3) : -14 -> -13], 
  [(1,2,4) : -14 -> -12], [(1,3,4) : -14 -> -11] ]
gap> hom8 := GroupoidHomomorphismFromSinglePiece( Gd8, Kk4, gen1, images );
groupoid homomorphism : Gd8 -> Kk4
[ [ [(5,6,7,8) : -9 -> -9], [(5,7) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7], [() : -9 -> -6] ], 
  [ [(1,4)(2,3) : -14 -> -14], [() : -14 -> -14], [(1,2,3) : -14 -> -13], 
      [(1,2,4) : -14 -> -12], [(1,3,4) : -14 -> -11] ] ]
gap> gend8 := GeneratorsOfGroup( d8 );;
gap> imh := [ (1,4)(2,3), () ];;
gap> h := GroupHomomorphismByImages( d8, a4, gend8, imh );                     
[ (5,6,7,8), (5,7) ] -> [ (1,4)(2,3), () ]
gap> hom9 := GroupoidHomomorphism( Gd8, Kk4, h, [-14,-13,-12,-11],
>                 [ (), (1,2,3), (1,2,4), (1,3,4) ] );;
gap> hom8 = hom9;
true
gap> e1 := Arrow( Gd8, (5,6,7,8), -7, -8 );;
gap> ImageElm( hom8, e1 );
[(1,2,4) : -12 -> -13]
gap> IsGroupoidHomomorphism( hom8 );
true

## Section 5.2
## SubSection 5.2.1
gap> [ IsInjectiveOnObjects( hom8 ), IsSurjectiveOnObjects( hom8 ) ]; 
[ true, true ]
gap> [ IsInjective( hom8 ), IsSurjective( hom8 ) ];
[ false, false ]
gap> ad8 := GroupHomomorphismByImages( d8, d8, 
>               [ (5,6,7,8), (5,7) ], [ (5,8,7,6), (6,8) ] );; 
gap> md8 := GroupoidHomomorphism( Gd8, Gd8, ad8,
>               [-6,-9,-8,-7], [(),(5,7),(6,8),(5,7)(6,8)] );
groupoid homomorphism : Gd8 -> Gd8
[ [ [(5,6,7,8) : -9 -> -9], [(5,7) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7], [() : -9 -> -6] ], 
  [ [(5,8,7,6) : -6 -> -6], [(6,8) : -6 -> -6], [(5,7) : -6 -> -9], 
      [(6,8) : -6 -> -8], [(5,7)(6,8) : -6 -> -7] ] ]
gap> IsBijectiveOnObjects( md8 );
true
gap> [ IsInjective( md8 ), IsSurjective( md8 ) ];  
[ true, true ]
gap> [ IsEndomorphismWithObjects( md8 ), IsAutomorphismWithObjects( md8 ) ];
[ true, true ]

## SubSection 5.2.3
gap> RootGroupHomomorphism( hom8 );
[ (5,6,7,8), (5,7) ] -> [ (1,4)(2,3), () ]
gap> ImagesOfObjects( hom8 );
[ -14, -13, -12, -11 ]
gap> ImageElementsOfRays( hom8 );
[ (), (1,2,3), (1,2,4), (1,3,4) ]

## SubSection 5.2.6
gap> MappingGeneratorsImages( hom8 );
[ [ [(5,6,7,8) : -9 -> -9], [(5,7) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7], [() : -9 -> -6] ], 
  [ [(1,4)(2,3) : -14 -> -14], [() : -14 -> -14], [(1,2,3) : -14 -> -13], 
      [(1,2,4) : -14 -> -12], [(1,3,4) : -14 -> -11] ] ]
gap> MappingToSinglePieceData( hom8 );
[ [ [ (5,6,7,8), (5,7) ] -> [ (1,4)(2,3), () ], [ -14, -13, -12, -11 ], 
      [ (), (1,2,3), (1,2,4), (1,3,4) ] ] ]

## SubSection 5.2.7
gap> ObjectGroupHomomorphism( hom8, -8 );
[ (5,6,7,8), (5,7) ] -> [ (1,3)(2,4), () ]

## Section 5.3
## SubSection 5.3.1
gap> incKk4 := InclusionMappingGroupoids( Ha4, Kk4 );     
groupoid homomorphism : Kk4 -> Ha4
[ [ [(1,2)(3,4) : -14 -> -14], [(1,3)(2,4) : -14 -> -14], 
      [(1,2,3) : -14 -> -13], [(1,2,4) : -14 -> -12], [(1,3,4) : -14 -> -11] ],
  [ [(1,2)(3,4) : -14 -> -14], [(1,3)(2,4) : -14 -> -14], 
      [(1,2,3) : -14 -> -13], [(1,2,4) : -14 -> -12], [(1,3,4) : -14 -> -11]
    ] ]

## SubSection 5.3.2
gap> Hd8 := Subgroupoid( Gd8, [-9,-8,-7] );;
gap> Hc4 := Subgroupoid( Hd8, c4 );  SetName( Hc4, "Hc4" );
single piece groupoid: < c4, [ -9, -8, -7 ] >
gap> res4 := RestrictedMappingGroupoids( hom8, Hc4 );
groupoid homomorphism : Hc4 -> Kk4
[ [ [(5,6,7,8) : -9 -> -9], [() : -9 -> -8], [() : -9 -> -7] ], 
  [ [(1,4)(2,3) : -14 -> -14], [(1,2,3) : -14 -> -13], [(1,2,4) : -14 -> -12]
      ] ]
gap> ParentMappingGroupoids( res4 ) = hom8; 
true
gap> Range( res4 );
Kk4
gap> ImagesSource( res4 );
single piece groupoid with rays: < Group( [ (1,4)(2,3) ] ), [ -14, -13, -12 ],
    [ (), (1,2,3), (1,2,4) ] >

## SubSection 5.3.3
gap> isoHa4 := IsomorphismNewObjects( Ha4, [-30,-29,-28,-27] );
groupoid homomorphism : 
[ [ [(1,2,3) : -14 -> -14], [(2,3,4) : -14 -> -14], [() : -14 -> -13], 
      [() : -14 -> -12], [() : -14 -> -11] ], 
  [ [(1,2,3) : -30 -> -30], [(2,3,4) : -30 -> -30], [() : -30 -> -29], 
      [() : -30 -> -28], [() : -30 -> -27] ] ]
gap> Ka4 := Range( isoHa4 );  SetName( Ka4, "Ka4" );
single piece groupoid: < a4, [ -30, -29, -28, -27 ] >
gap> IsSubgroupoid( Gk4, Kk4 );
true
gap> incHa4 := InclusionMappingGroupoids( Ga4, Ha4 );; 
gap> mor8 := hom8 * incKk4 * incHa4;
groupoid homomorphism : Gd8 -> Ga4
[ [ [(5,6,7,8) : -9 -> -9], [(5,7) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7], [() : -9 -> -6] ], 
  [ [(1,4)(2,3) : -14 -> -14], [() : -14 -> -14], [(1,2,3) : -14 -> -13], 
      [(1,2,4) : -14 -> -12], [(1,3,4) : -14 -> -11] ] ]
gap> ImageElm( mor8, e1 );
[(1,2,4) : -12 -> -13]

## SubSection 5.3.4
gap> isoGk4 := IsomorphismStandardGroupoid( Gk4, [-45..-41] );
groupoid homomorphism : 
[ [ [(1,2)(3,4) : -15 -> -15], [(1,3)(2,4) : -15 -> -15], 
      [(1,2,3) : -15 -> -14], [(1,2,4) : -15 -> -13], [(1,3,4) : -15 -> -12], 
      [(2,3,4) : -15 -> -11] ], 
  [ [(1,2)(3,4) : -45 -> -45], [(1,3)(2,4) : -45 -> -45], [() : -45 -> -44], 
      [() : -45 -> -43], [() : -45 -> -42], [() : -45 -> -41] ] ]
gap> e5 := Arrow( Gk4, (1,2,4) , -13, -12 );
[(1,2,4) : -13 -> -12]
gap> ImageElm( isoGk4, e5 );
[(1,3)(2,4) : -43 -> -42]
gap> invGk4 := InverseGeneralMapping( isoGk4 );
groupoid homomorphism : 
[ [ [(1,2)(3,4) : -45 -> -45], [(1,3)(2,4) : -45 -> -45], [() : -45 -> -44], 
      [() : -45 -> -43], [() : -45 -> -42], [() : -45 -> -41] ], 
  [ [(1,2)(3,4) : -15 -> -15], [(1,3)(2,4) : -15 -> -15], 
      [(1,2,3) : -15 -> -14], [(1,2,4) : -15 -> -13], [(1,3,4) : -15 -> -12], 
      [(2,3,4) : -15 -> -11] ] ]

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
gap> isoGq8 := IsomorphismPermGroupoid( Gq8 );;
gap> regGq8 := RegularActionHomomorphismGroupoid( Gq8 );
groupoid homomorphism : 
[ [ [x : -19 -> -19], [y : -19 -> -19], [y2 : -19 -> -19], 
      [<identity> of ... : -19 -> -18], [<identity> of ... : -19 -> -17] ], 
  [ [(1,2,4,6)(3,8,7,5) : -19 -> -19], [(1,3,4,7)(2,5,6,8) : -19 -> -19], 
      [(1,4)(2,6)(3,7)(5,8) : -19 -> -19], [() : -19 -> -18], 
      [() : -19 -> -17] ] ]
gap> Pq8 := Range( regGq8 );  SetName( Pq8, "Pq8" );
single piece groupoid: < Group( [ (1,2,4,6)(3,8,7,5), (1,3,4,7)(2,5,6,8), 
  (1,4)(2,6)(3,7)(5,8) ] ), [ -19, -18, -17 ] >
gap> e7 := Arrow( Gq8, x*y, -18, -17 );;
gap> ImageElm( regGq8, e7 );
[(1,5,4,8)(2,7,6,3) : -18 -> -17]
gap> isoHc4 := IsomorphismPcGroupoid( Hc4 );
groupoid homomorphism : 
[ [ [(5,6,7,8) : -9 -> -9], [() : -9 -> -8], [() : -9 -> -7] ], 
  [ [f1 : -9 -> -9], [<identity> of ... : -9 -> -8], 
      [<identity> of ... : -9 -> -7] ] ]

## SubSection 5.4.1
gap> V2 := UnionOfPieces( Gq8, Hc4 );;
gap> genPq8 := GeneratorsOfGroupoid( Pq8 );;
gap> imHc4 := [ genPq8[1], genPq8[4], genPq8[5] ];
[ [(1,2,4,6)(3,8,7,5) : -19 -> -19], [() : -19 -> -18], [() : -19 -> -17] ]
gap> genHc4 := GeneratorsOfGroupoid( Hc4 );;
gap> homHc4 := GroupoidHomomorphism( Hc4, Pq8, genHc4, imHc4 );
groupoid homomorphism : Hc4 -> Pq8
[ [ [(5,6,7,8) : -9 -> -9], [() : -9 -> -8], [() : -9 -> -7] ], 
  [ [(1,2,4,6)(3,8,7,5) : -19 -> -19], [() : -19 -> -18], [() : -19 -> -17] ] ]
gap> homV2 := HomomorphismToSinglePiece( V2, Pq8, [ regGq8, homHc4 ] );
groupoid homomorphism : 
[ [ [ [x : -19 -> -19], [y : -19 -> -19], [y2 : -19 -> -19], 
          [<identity> of ... : -19 -> -18], [<identity> of ... : -19 -> -17] ], 
      [ [(1,2,4,6)(3,8,7,5) : -19 -> -19], [(1,3,4,7)(2,5,6,8) : -19 -> -19], 
          [(1,4)(2,6)(3,7)(5,8) : -19 -> -19], [() : -19 -> -18], 
          [() : -19 -> -17] ] ], 
  [ [ [(5,6,7,8) : -9 -> -9], [() : -9 -> -8], [() : -9 -> -7] ], 
      [ [(1,2,4,6)(3,8,7,5) : -19 -> -19], [() : -19 -> -18], 
          [() : -19 -> -17] ] ] ]
gap> ImageElm( homV2, e7 );            
[(1,5,4,8)(2,7,6,3) : -18 -> -17]

## SubSection 5.4.2, Homomorphisms from homogeneous discrete 
gap> c3a := Subgroup( a4, [(1,2,3)] );; c3b := Subgroup( a4, [(1,2,4)] );;
gap> c3c := Subgroup( a4, [(1,3,4)] );; c3d := Subgroup( a4, [(2,3,4)] );;
gap> hc6a := GroupHomomorphismByImages( c6, c3a, 
>                [(11,12,13)(14,15)], [(1,2,3)] );;
gap> hc6b := GroupHomomorphismByImages( c6, c3b, 
>                [(11,12,13)(14,15)], [(1,2,4)] );;
gap> hc6c := GroupHomomorphismByImages( c6, c3c, 
>                [(11,12,13)(14,15)], [(1,3,4)] );;
gap> hc6d := GroupHomomorphismByImages( c6, c3d, 
>                [(11,12,13)(14,15)], [(2,3,4)] );;
gap> mor6 := GroupoidHomomorphismFromHomogeneousDiscrete( HDc6, Ga4,
>              [ hc6a, hc6b, hc6c, hc6d ], [-15,-14,-12,-11] );
groupoid homomorphism : HDc6 -> Ga4
gap> e6 := Arrow( HDc6, (11,12,13), -25, -25 );;
gap> ImageElm( mor6, e6 );
[(1,3,4) : -12 -> -12]

## Section 5.5, Homomorphisms with more than one piece 

## SubSection 5.5.1
gap> W1 := UnionOfPieces( Ha4, Gd8 );;
gap> W2 := UnionOfPieces( Ka4, Kk4 );;
gap> SetName( W1, "[Ha4,Gd8]" );  SetName( W2, "[Ka4,Kk4]" );
gap> homW := HomomorphismByUnion( W1, W2, [ isoHa4, hom8 ] );;
gap> Display( homW );           
groupoid homomorphism: [Ha4,Gd8] -> [Ka4,Kk4] with pieces :
homomorphism to single piece groupoid: Ha4 -> Ka4
root group homomorphism:
(1,2,3) -> (1,2,3)
(2,3,4) -> (2,3,4)
object map: [ -14, -13, -12, -11 ] -> [ -30, -29, -28, -27 ]
ray images: [ (), (), (), () ]
homomorphism to single piece groupoid: Gd8 -> Kk4
root group homomorphism:
(5,6,7,8) -> (1,4)(2,3)
(5,7) -> ()
object map: [ -9, -8, -7, -6 ] -> [ -14, -13, -12, -11 ]
ray images: [ (), (1,2,3), (1,2,4), (1,3,4) ]


## SubSection 5.5.2
gap> s3b := Group( (4,6,8)(5,7,9), (4,9)(5,8)(6,7) );;
gap> s3c := Group( (4,6,8)(5,7,9), (5,9)(6,8) );;
gap> Gb := SinglePieceGroupoid( s3b, [-6,-5,-4] );; 
gap> Gc := SinglePieceGroupoid( s3c, [-16,-15,-14] );; 
gap> SetName( Gb, "Gb" );  SetName( Gc, "Gc" );  
gap> c6b := Group( (1,2,3,4,5,6) );; 
gap> c6c := Group( (7,8)(9,10,11) );;
gap> Hb := SinglePieceGroupoid( c6b, [-10,-9,-8,-7] );; 
gap> Hc := SinglePieceGroupoid( c6c, [-20,-19,-18,-17] );; 
gap> SetName( Hb, "Hb" );  SetName( Hc, "Hc" ); 
gap> IsomorphismGroupoids( Gb, Gc );
groupoid homomorphism : Gb -> Gc
[ [ [(4,6,8)(5,7,9) : -6 -> -6], [(4,9)(5,8)(6,7) : -6 -> -6], 
      [() : -6 -> -5], [() : -6 -> -4] ], 
  [ [(4,6,8)(5,7,9) : -16 -> -16], [(5,9)(6,8) : -16 -> -16], 
      [() : -16 -> -15], [() : -16 -> -14] ] ]
gap> IsomorphismGroupoids( Gb, Hb );
fail
gap> B := UnionOfPieces( [ Gb, Hb ] );; 
gap> C := UnionOfPieces( [ Gc, Hc ] );;
gap> isoBC := IsomorphismGroupoids( B, C );; 
gap> Print( List( PiecesOfMapping(isoBC), p -> [Source(p),Range(p)] ) );
[ [ Hb, Hc ], [ Gb, Gc ] ]

gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
gap> STOP_TEST( "gpdhom.tst", 10000 );
