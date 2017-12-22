##############################################################################
##
#W  coset-test.tst                Groupoids Package              Chris Wensley
##
#Y  Copyright (C) 2000-2017, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

gap> s5gens := [ (1,2,3,4,5), (4,5) ];;
gap> s5 := Group( s5gens );;
gap> SetName( s5, "s5" );;
gap> obs := [-9,-8,-7,-6,-5];;
gap> Gs5 := SinglePieceGroupoid( s5, obs );;
gap> s4 := Subgroup( s5, [ (1,2,3,4), (3,4) ] );;
gap> SetName( s4, "s4" );
gap> Hs4 := SubgroupoidWithRays( Gs5, s4, [ (), (4,5), (3,5), (2,5), (1,5) ] );
single piece groupoid with rays: < s4, [ -9, -8, -7, -6, -5 ], 
[ (), (4,5), (3,5), (2,5), (1,5) ] >

gap> star6 := ObjectStar( Hs4, -6 ); 
<star at -6 with group Group( [ (1,5,3,4), (3,4) ] )>
gap> a6 := Arrow( Hs4, (1,2,4,3,5), -6, -8 );;
gap> a6 in star6; 
true
gap> costar7 := ObjectCostar( Hs4, -7 ); 
<costar at -7 with group Group( [ (1,2,5,4), (4,5) ] )>
gap> a7 := Arrow( Hs4, (1,3,4,5,2), -5, -7 );;
gap> a7 in costar7;
true
gap> hs85 := Homset( Hs4, -8, -5 ); 
<homset -8 -> -5 with group Group( [ (1,2,3,5), (3,5) ] )>
gap> a8 := Arrow( Hs4, (1,3,5,2,4), -8, -5 );;
gap> a8 in hs85;
true

gap> c3 := Subgroup( s5, [ (1,2,3) ] );; 
gap> c4 := Subgroup( s5, [ (1,2,5,4) ] );; 
gap> s3 := Subgroup( s5, [ (3,4), (4,5) ] );; 
gap> SetName( c3, "c3" );  SetName( c4, "c4" );  SetName( s3, "s3" ); 
gap> Us4 := Subgroupoid( Hs4, [ [ c3, [-9,-8,-5], [(),(1,2)(4,5),(1,5)(2,4)] ], 
>                               [ c4, [-7,-6],[(),(1,4,5,3,2)] ] ] );; 
gap> Display( Us4 ); 
groupoid with 2 pieces:
<     objects: [ -9, -8, -5 ]
   parent gpd: single piece groupoid: < s5, [ -9, -8, -5 ] >
   root group: c3 = <[ (1,2,3) ]>
  conjugators: [ (), (1,2)(4,5), (1,5)(2,4) ]
<     objects: [ -7, -6 ]
   parent gpd: single piece groupoid: < s5, [ -7, -6 ] >
   root group: c4 = <[ (1,2,5,4) ]>
  conjugators: [ (), (1,4,5,3,2) ]
gap> IsWideSubgroupoid( Gs5, Us4 );
true


gap> ## reps := RightCosetRepresentatives( Hs4, Us4 );

gap> a1 := Arrow( Hs4, (1,2,4), -8, -5 );; 
gap> a1 in Us4; 
false
gap> rc1 := RightCoset( Gs5, Us4, a1 ); 
<right coset of ((1,2,4) : -8 -> -5) >
gap> for x in rc1 do Print(x,"\n"); od;
[(1,2) : -9 -> -8]
[(1,2) : -8 -> -8]
[(1,3) : -9 -> -8]
[(1,3) : -8 -> -8]
[(2,3) : -9 -> -8]
[(2,3) : -8 -> -8]
gap> a2 := Arrow( Gs5, (2,3), -9, -8 );; 
gap> a2 in rc1; 
false - incorrect!
gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 

#############################################################################
##
#E  coset-test.tst . . . . . . . . . . . . . . . . . . . . . . . .  ends here
