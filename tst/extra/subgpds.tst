############################################################################
##
#W  subgpds.tst                Groupoids Package               Chris Wensley
##

gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

gap> s4 := Group( (1,2), (2,3), (3,4) );; 
gap> SetName( s4, "s4" );
gap> a4 := Subgroup( s4, [ (1,2,3), (2,3,4) ] );; 
gap> SetName( a4, "a4" ); 
gap> Gs4 := Groupoid( s4, [-9..-1] );; 
gap> SetName( Gs4, "Gs4" ); 
gap> Hs4 := Groupoid( s4, [-19..-11] );; 
gap> SetName( Hs4, "Hs4" ); 
gap> iso := GroupHomomorphismByImages( s4, s4, 
>               [(1,2),(2,3),(3,4)], [(2,3),(3,4),(1,4)] );; 
gap> mor := GroupoidHomomorphism( Gs4, Hs4, iso, 
>               [-11,-12,-13,-14,-15,-16,-17,-18,-19], 
>               [(),(),(),(),(),(),(),(),()] );; 
gap> a := Arrow( Gs4, (1,2,4,3), -9, -8 ); 
[(1,2,4,3) : -9 -> -8]
gap> ia := ImageElm( mor, a );
[(1,4,2,3) : -11 -> -12]
gap> Fa4 := SubgroupoidByPieces( Gs4, [ 
>               [ a4, [-9..-6], [(),(1,2),(1,3),(1,4)] ],
>               [ a4, [-4..-1], [(),(2,3),(2,4),(3,4)] ] ] );
groupoid with 2 pieces:
1:  single piece groupoid with rays: < a4, [ -9 .. -6 ], 
[ (), (1,2), (1,3), (1,4) ] >
2:  single piece groupoid with rays: < a4, [ -4 .. -1 ], 
[ (), (2,3), (2,4), (3,4) ] >
gap> SetName( Fa4, "Fa4" );
gap> Ea4 := Pieces( Fa4 )[1];
single piece groupoid with rays: < a4, [ -9 .. -6 ], 
[ (), (1,2), (1,3), (1,4) ] >
gap> SetName( Ea4, "Ea4" );
gap> Da4 := Pieces( Fa4 )[2];
single piece groupoid with rays: < a4, [ -4 .. -1 ], 
[ (), (2,3), (2,4), (3,4) ] >
gap> SetName( Da4, "Da4" );
gap> resF := RestrictedMappingGroupoids( mor, Fa4 );
groupoid homomorphism from several pieces : 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [(1,2) : -9 -> -8], 
      [(1,3) : -9 -> -7], [(1,4) : -9 -> -6] ], 
  [ [(2,3,4) : -11 -> -11], [(1,3,4) : -11 -> -11], [(2,3) : -11 -> -12], 
      [(2,4) : -11 -> -13], [(1,2) : -11 -> -14] ] ]
groupoid homomorphism : 
[ [ [(1,2,3) : -4 -> -4], [(2,3,4) : -4 -> -4], [(2,3) : -4 -> -3], 
      [(2,4) : -4 -> -2], [(3,4) : -4 -> -1] ], 
  [ [(2,3,4) : -16 -> -16], [(1,3,4) : -16 -> -16], [(3,4) : -16 -> -17], 
      [(1,3) : -16 -> -18], [(1,4) : -16 -> -19] ] ]
gap> Ka4 := Range( resF );
groupoid with 2 pieces:
1:  single piece groupoid with rays: < Group( [ (1,2,3), (1,4,3) ] ), 
[ -19, -18, -17, -16 ], [ (), (1,4,3), (1,3,4), (1,4) ] >
2:  single piece groupoid with rays: < Group( [ (1,3,4), (2,3,4) ] ), 
[ -14, -13, -12, -11 ], [ (), (1,4,2), (1,3,2), (1,2) ] >
gap> SetName( Ka4, "Ka4" );
gap> b := Arrow( Fa4, (1,2,3), -7, -8 );;
gap> ImageElm( resF, b );
[(2,3,4) : -13 -> -12]
gap> c3 := Subgroup( a4, [(2,3,4)] );;
gap> SetName( c3, "c3" );
gap> Ec3 := SubgroupoidBySubgroup( Ea4, c3 );
single piece groupoid with rays: < c3, [ -9 .. -6 ], 
[ (), (1,2), (1,3), (1,4) ] >
gap> SetName( Ec3, "Ec3" );
gap> Dc3 := SubgroupoidBySubgroup( Da4, c3 );
single piece groupoid with rays: < c3, [ -4 .. -1 ], 
[ (), (2,3), (2,4), (3,4) ] >
gap> SetName( Dc3, "Dc3" );
gap> Uc3 := UnionOfPieces( Ec3, Dc3 );
groupoid with 2 pieces:
[ Ec3, Dc3 ]
gap> SetName( Uc3, "Uc3" );
gap> IsSubgroupoid( Fa4, Uc3 );
true
gap> resU := RestrictedMappingGroupoids( resF, Uc3 );
groupoid homomorphism from several pieces : 
groupoid homomorphism : 
[ [ [(2,3,4) : -9 -> -9], [(1,2) : -9 -> -8], [(1,3) : -9 -> -7], 
      [(1,4) : -9 -> -6] ], 
  [ [(1,3,4) : -11 -> -11], [(2,3) : -11 -> -12], [(2,4) : -11 -> -13], 
      [(1,2) : -11 -> -14] ] ]
groupoid homomorphism : 
[ [ [(2,3,4) : -4 -> -4], [(2,3) : -4 -> -3], [(2,4) : -4 -> -2], 
      [(3,4) : -4 -> -1] ], 
  [ [(1,3,4) : -16 -> -16], [(3,4) : -16 -> -17], [(1,3) : -16 -> -18], 
      [(1,4) : -16 -> -19] ] ]

gap> Ca4 := SubgroupoidByObjects( Fa4, [-8,-7,-3,-2] ); 
groupoid with 2 pieces:
1:  single piece groupoid with rays: < Group( [ (1,3,2), (1,3,4) ] ), 
[ -8, -7 ], [ (), (1,2,3) ] >
2:  single piece groupoid with rays: < Group( [ (1,3,2), (2,4,3) ] ), 
[ -3, -2 ], [ (), (2,3,4) ] >
gap> SetName( Ca4, "Ca4" );
gap> resC := RestrictedMappingGroupoids( resF, Ca4 );
groupoid homomorphism from several pieces : 
groupoid homomorphism : 
[ [ [(1,3,2) : -8 -> -8], [(1,3,4) : -8 -> -8], [(1,2,3) : -8 -> -7] ], 
  [ [(2,4,3) : -12 -> -12], [(1,2,4) : -12 -> -12], [(2,3,4) : -12 -> -13] ] ]
groupoid homomorphism : 
[ [ [(1,3,2) : -3 -> -3], [(2,4,3) : -3 -> -3], [(2,3,4) : -3 -> -2] ], 
  [ [(2,4,3) : -17 -> -17], [(1,4,3) : -17 -> -17], [(1,3,4) : -17 -> -18] ] ]
gap> Ma4 := MaximalDiscreteSubgroupoid( Ca4 ); 
homogeneous, discrete groupoid: < Group( [ (1,3,2), (1,3,4) ] ), 
[ -8, -7, -3, -2 ] >
gap> SetName( Ma4, "Ma4" );
gap> resM := RestrictedMappingGroupoids( resC, Ma4 );
groupoid homomorphism from several pieces : 
groupoid homomorphism : 
[ [ [(1,3,2) : -8 -> -8], [(1,3,4) : -8 -> -8] ], 
  [ [(2,4,3) : -12 -> -12], [(1,2,4) : -12 -> -12] ] ]
groupoid homomorphism : 
[ [ [(1,3,2) : -7 -> -7], [(1,3,4) : -7 -> -7] ], 
  [ [(2,4,3) : -13 -> -13], [(1,2,4) : -13 -> -13] ] ]
groupoid homomorphism : 
[ [ [(1,3,2) : -3 -> -3], [(1,3,4) : -3 -> -3] ], 
  [ [(2,4,3) : -17 -> -17], [(1,2,4) : -17 -> -17] ] ]
groupoid homomorphism : 
[ [ [(1,3,2) : -2 -> -2], [(1,3,4) : -2 -> -2] ], 
  [ [(2,4,3) : -18 -> -18], [(1,2,4) : -18 -> -18] ] ]
gap> Mc3 := MaximalDiscreteSubgroupoid( Uc3 ); 
groupoid with 8 pieces:
1:  single piece groupoid: < c3, [ -9 ] >
2:  single piece groupoid: < Group( [ (1,3,4) ] ), [ -8 ] >
3:  single piece groupoid: < Group( [ (1,4,2) ] ), [ -7 ] >
4:  single piece groupoid: < Group( [ (1,2,3) ] ), [ -6 ] >
5:  single piece groupoid: < c3, [ -4 ] >
6:  single piece groupoid: < Group( [ (2,4,3) ] ), [ -3 ] >
7:  single piece groupoid: < Group( [ (2,4,3) ] ), [ -2 ] >
8:  single piece groupoid: < Group( [ (2,4,3) ] ), [ -1 ] >
gap> SetName( Mc3, "Mc3" );

gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
