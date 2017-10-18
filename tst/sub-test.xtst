##############################################################################
##
#W  sub-test.xtst              Groupoids Package                 Chris Wensley
##
#Y  Copyright (C) 2000-2017, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
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
gap> a := Arrow( Gs4, (1,2,3), -9, -8 ); 
[(1,2,3) : -9 -> -8]
gap> ia := ImageElm( mor, a );
[(2,3,4) : -11 -> -12]
gap> Fa4 := SubgroupoidByPieces( Gs4, [ [a4,[-9..-7]], [a4,[-3..-1]] ] );
groupoid with 2 pieces:
1:  single piece groupoid: < a4, [ -9, -8, -7 ] >
2:  single piece groupoid: < a4, [ -3, -2, -1 ] >
gap> res := RestrictedMappingGroupoids( mor, Fa4 );;
gap> Range(res);
groupoid with 2 pieces:
1:  single piece groupoid: < Group( [ (2,3,4), (1,3,4) ] ), [ -19, -18, -17 
 ] >
2:  single piece groupoid: < Group( [ (2,3,4), (1,3,4) ] ), [ -13, -12, -11 
 ] >
gap> ImageElm( res, a );
[(2,3,4) : -11 -> -12]
gap> c3 := Subgroup( a4, [(2,3,4)] );;
gap> SetName( c3, "c3" );
gap> Ec3 := SubgroupoidByPieces( Fa4, 
>               [ [c3,[-9]], [c3,[-7]], [c3,[-3]], [c3,[-1]] ] );
groupoid with 4 pieces:
1:  single piece groupoid: < c3, [ -9 ] >
2:  single piece groupoid: < c3, [ -7 ] >
3:  single piece groupoid: < c3, [ -3 ] >
4:  single piece groupoid: < c3, [ -1 ] >
gap> res2 := RestrictedMappingGroupoids( res, Ec3 );;
gap> ImageElm( res2, ArrowNC( true, (2,3,4), -7, -7 ) );  
[(1,3,4) : -13 -> -13]

#############################################################################
##
#E  sub-test.xtst . . . . . . . . . . . . . . . . . . . . . . . . . ends here
