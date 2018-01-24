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

gap> star7 := ObjectStar( Hs4, -7 ); 
<star at -7 with group Group( [ (1,2,5,4), (4,5) ] )>
gap> PrintSelection( star7, 24, 24 );
24 : [(2,4,3) : -7 -> -6]
48 : [(1,5,4,2) : -7 -> -7]
72 : [(1,5)(3,4) : -7 -> -8]
96 : [(1,2)(3,5) : -7 -> -9]
120 : [(1,2,3)(4,5) : -7 -> -5]
gap> a76 := Arrow( Hs4, (2,5,3), -7, -6 );;
gap> a76 in star7; 
true
gap> costar6 := ObjectCostar( Hs4, -6 ); 
<costar at -6 with group Group( [ (1,5,3,4), (3,4) ] )>
gap> PrintSelection( costar6, 24, 24 );
24 : [(3,5,4) : -6 -> -6]
48 : [(1,3,2,4) : -7 -> -6]
72 : [(1,4,2,5,3) : -8 -> -6]
96 : [(1,5,2,3,4) : -9 -> -6]
120 : [(1,2,4,3) : -5 -> -6]
gap> a76 in costar6;
true
gap> hs85 := Homset( Hs4, -8, -5 ); 
<homset -8 -> -5 with group Group( [ (1,2,3,5), (3,5) ] )>
gap> PrintSelection( hs85, 1, 6 );
1 : [(1,5,4) : -8 -> -5]
7 : [(1,3,5,2,4) : -8 -> -5]
13 : [(1,4)(2,3) : -8 -> -5]
19 : [(1,2,5,3,4) : -8 -> -5]
gap> a85 := Arrow( Hs4, (1,4), -8, -5 );;
gap> a85 in hs85;
true
gap> a76 in hs85;
false

gap> c3 := Subgroup( s5, [ (1,2,3) ] );; 
gap> c4 := Subgroup( s5, [ (1,2,5,4) ] );; 
gap> SetName( c3, "c3" );  SetName( c4, "c4" ); 
gap> Uc3c4 := Subgroupoid( Hs4, 
>                 [ [ c3, [-9,-8,-5], [ (), (1,3,5,4), (1,2,4,5) ] ], 
>                   [ c4, [-7,-6], [ (), (1,4,3,2,5) ] ] ] );; 
gap> Display( Uc3c4 ); 
groupoid with 2 pieces:
<     objects: [ -9, -8, -5 ]
   parent gpd: single piece groupoid: < s5, [ -9, -8, -5 ] >
   root group: c3 = <[ (1,2,3) ]>
  conjugators: [ (), (1,3,5,4), (1,2,4,5) ]
<     objects: [ -7, -6 ]
   parent gpd: single piece groupoid: < s5, [ -7, -6 ] >
   root group: c4 = <[ (1,2,5,4) ]>
  conjugators: [ (), (1,4,3,2,5) ]
gap> IsWideSubgroupoid( Gs5, Uc3c4 );
true

gap> hs85U := Homset( Uc3c4, -8, -5 ); 
<homset -8 -> -5 with group Group( [ (2,5,3) ] )>
gap> PrintOneItemPerLine( hs85U );
[(1,5,3,2,4) : -8 -> -5]
[(1,5,4) : -8 -> -5]
[(1,5,2,3,4) : -8 -> -5]
gap> a85 in Uc3c4; 
false
gap> rc85 := RightCoset( Hs4, Uc3c4, a85 ); 
<right coset of single piece groupoid with rays: < c3, [ -9, -8, -5 ], 
[ (), (1,3,5,4), (1,2,4,5) ] > with representative [(4,5) : -5 -> -5]>
gap> PrintOneItemPerLine( rc85 );
[(1,3,5) : -9 -> -5]
[(1,4) : -8 -> -5]
[(2,3,5,4) : -5 -> -5]
[(1,4,2,3,5) : -9 -> -5]
[(1,2,3,4) : -8 -> -5]
[(2,4,3,5) : -5 -> -5]
[(1,2,4,3,5) : -9 -> -5]
[(1,3,2,4) : -8 -> -5]
[(3,5) : -5 -> -5]
gap> b85 := Arrow( Hs4, (1,5), -9, -5 );; 
gap> b85 in rc85; 
false
gap> lc85 := LeftCoset( Hs4, Uc3c4, a85 ); 
<left coset of single piece groupoid with rays: < c3, [ -9, -8, -5 ], 
[ (), (1,3,5,4), (1,2,4,5) ] > with representative [(1,2,3,5) : -8 -> -8]>
gap> PrintOneItemPerLine( lc85 );         
[(1,2)(4,5) : -8 -> -9]
[(1,2,3,5) : -8 -> -8]
[(1,4) : -8 -> -5]
[(1,2,4,5,3) : -8 -> -9]
[(1,2) : -8 -> -8]
[(1,4)(2,5,3) : -8 -> -5]
[(1,2,3,4,5) : -8 -> -9]
[(1,2,5,3) : -8 -> -8]
[(1,4)(2,3,5) : -8 -> -5]
gap> b85 in lc85; 
false

gap> reps := RightCosetRepresentatives( Hs4, Uc3c4 );
[ [() : -9 -> -9], [(2,3) : -9 -> -9], [(1,3)(2,4) : -9 -> -9], 
  [(1,3,4,2) : -9 -> -9], [(1,4)(2,3) : -9 -> -9], [(1,4) : -9 -> -9], 
  [(1,2)(3,4) : -9 -> -9], [(1,2,4,3) : -9 -> -9], [() : -8 -> -8], 
  [(3,5) : -8 -> -8], [(1,3)(2,5) : -8 -> -8], [(1,3,2,5) : -8 -> -8], 
  [(1,5)(2,3) : -8 -> -8], [(1,5,2,3) : -8 -> -8], [(1,2)(3,5) : -8 -> -8], 
  [(1,2) : -8 -> -8], [() : -5 -> -5], [(3,4) : -5 -> -5], 
  [(2,4)(3,5) : -5 -> -5], [(2,4,5,3) : -5 -> -5], [(2,3)(4,5) : -5 -> -5], 
  [(2,3,5,4) : -5 -> -5], [(2,5)(3,4) : -5 -> -5], [(2,5) : -5 -> -5], 
  [() : -7 -> -7], [(4,5) : -7 -> -7], [(2,4) : -7 -> -7], 
  [(2,4,5) : -7 -> -7], [(2,5,4) : -7 -> -7], [(2,5) : -7 -> -7], 
  [() : -6 -> -6], [(4,5) : -6 -> -6], [(3,4) : -6 -> -6], 
  [(3,4,5) : -6 -> -6], [(3,5,4) : -6 -> -6], [(3,5) : -6 -> -6] ]
gap> Length( reps );
36
gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 

#############################################################################
##
#E  coset-test.tst . . . . . . . . . . . . . . . . . . . . . . . .  ends here
