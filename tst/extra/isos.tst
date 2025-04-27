############################################################################
##
#W  isos.tst                   Groupoids Package               Chris Wensley
##

## SubSection 2.1.1 
gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

gap> s4a := Group( (1,2,3,4), (3,4) );;
gap> SetName( s4a, "s4a" );;
gap> a4a := Subgroup( s4a, [ (1,2,3), (2,3,4) ] );;
gap> SetName( a4a, "a4a" );; 
gap> Gs4a := Groupoid( s4a, [-5,-4,-3,-2,-1] );
single piece groupoid: < s4a, [ -5, -4, -3, -2, -1 ] >
gap> Hs4a := Subgroupoid( Gs4a, [ [ s4a, [-5,-4,-2,-1] ] ] );
single piece groupoid: < s4a, [ -5, -4, -2, -1 ] >
gap> Ja4a := SubgroupoidWithRays( Hs4a, a4a, [ (), (1,2,3,4), (1,3), (2,4) ] ); 
single piece groupoid with rays: < a4a, [ -5, -4, -2, -1 ], 
[ (), (1,2,3,4), (1,3), (2,4) ] >

gap> c2a4b := Group( [ (1,3,5)(2,4,6), (1,6,4)(2,5,3), (7,8) ] );;
gap> SetName( c2a4b, "c2a4b" );; 
gap> a4b := Subgroup( c2a4b, [ (1,3,5)(2,4,6), (1,6,4)(2,5,3) ] );; 
gap> SetName( a4b, "a4b" );;
gap> alpha := GroupHomomorphismByImages( a4a, a4b, 
>             [ (1,2,3), (2,3,4) ], [ (1,3,5)(2,4,6), (1,6,4)(2,5,3) ] );; 
gap> Hc2a4b := Groupoid( c2a4b, [-9,-8,-7,-6] );
single piece groupoid: < c2a4b, [ -9, -8, -7, -6 ] >
gap> Ja4b := SubgroupoidWithRays( Hc2a4b, a4b, 
>            [ (), (7,8), (1,3,5)(2,4,6)(7,8), (1,6,4)(2,5,3)(7,8) ] );
single piece groupoid with rays: < a4b, [ -9, -8, -7, -6 ], 
[ (), (7,8), (1,3,5)(2,4,6)(7,8), (1,6,4)(2,5,3)(7,8) ] >
gap> gensa := GeneratorsOfGroupoid( Ja4a );
[ [(1,2,3) : -5 -> -5], [(2,3,4) : -5 -> -5], [(1,2,3,4) : -5 -> -4], 
  [(1,3) : -5 -> -2], [(2,4) : -5 -> -1] ]
gap> imagb := [ Arrow( Ja4b, (1,3,5)(2,4,6), -7, -7 ), 
>           Arrow( Ja4b, (1,6,4)(2,5,3), -7, -7 ), 
>           Arrow( Ja4b, (1,2)(3,4)(7,8), -7, -9 ), 
>           Arrow( Ja4b, (1,4,5)(2,3,6), -7, -8 ),
>           Arrow( Ja4b, (3,4)(5,6), -7, -6 ) ]; 
[ [(1,3,5)(2,4,6) : -7 -> -7], [(1,6,4)(2,5,3) : -7 -> -7], 
  [(1,2)(3,4)(7,8) : -7 -> -9], [(1,4,5)(2,3,6) : -7 -> -8], 
  [(3,4)(5,6) : -7 -> -6] ]
gap> iso := GroupoidHomomorphismFromSinglePiece( Ja4a, Ja4b, gensa, imagb ); 
groupoid homomorphism : 
[ [ [(1,2,3) : -5 -> -5], [(2,3,4) : -5 -> -5], [(1,2,3,4) : -5 -> -4], 
      [(1,3) : -5 -> -2], [(2,4) : -5 -> -1] ], 
  [ [(1,3,5)(2,4,6) : -7 -> -7], [(1,6,4)(2,5,3) : -7 -> -7], 
      [(1,2)(3,4)(7,8) : -7 -> -9], [(1,4,5)(2,3,6) : -7 -> -8], 
      [(3,4)(5,6) : -7 -> -6] ] ]
gap> inv := iso^-1;
groupoid homomorphism : 
[ [ [(1,3,5)(2,4,6) : -9 -> -9], [(1,6,4)(2,5,3) : -9 -> -9], 
      [(7,8) : -9 -> -8], [(1,3,5)(2,4,6)(7,8) : -9 -> -7], 
      [(1,6,4)(2,5,3)(7,8) : -9 -> -6] ], 
  [ [(1,3,2) : -4 -> -4], [(1,4,2) : -4 -> -4], [(1,3,4) : -4 -> -2], 
      [(2,3) : -4 -> -5], [(2,4,3) : -4 -> -1] ] ]

gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
