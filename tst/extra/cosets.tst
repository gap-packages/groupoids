############################################################################
##
#W  cosets.tst                  Groupoids Package              Chris Wensley
##

gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

gap> s5gens := [ (1,2,3,4,5), (4,5) ];;
gap> s5 := Group( s5gens );;
gap> SetName( s5, "s5" );;
gap> obs := [-9,-8,-7,-6,-5];;
gap> Gs5 := SinglePieceGroupoid( s5, obs );;
gap> SetName( Gs5, "Gs5" ); 
gap> s4 := Subgroup( s5, [ (1,2,3,4), (3,4) ] );;
gap> SetName( s4, "s4" );
gap> Hs4 := SubgroupoidWithRays( Gs5, s4, [ (), (4,5), (3,5), (2,5), (1,5) ] );
single piece groupoid with rays: < s4, [ -9, -8, -7, -6, -5 ], 
[ (), (4,5), (3,5), (2,5), (1,5) ] >
gap> SetName( Hs4, "Hs4" ); 

gap> star7 := ObjectStar( Hs4, -7 ); 
<star at -7 with vertex group Group( [ (1,2,5,4), (4,5) ] )>
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
<costar at -6 with vertex group Group( [ (1,5,3,4), (3,4) ] )>
gap> PrintSelection( costar6, 24, 24 );
24 : [(3,5,4) : -6 -> -6]
48 : [(1,3,2,4) : -7 -> -6]
72 : [(1,4,2,5,3) : -8 -> -6]
96 : [(1,5,2,3,4) : -9 -> -6]
120 : [(1,2,4,3) : -5 -> -6]
gap> a76 in costar6;
true
gap> hs85 := Homset( Hs4, -8, -5 ); 
<homset -8 -> -5 with head group Group( [ (2,3,4,5), (3,4) ] )>
gap> PrintSelection( hs85, 1, 6 );
1 : [(1,5,4) : -8 -> -5]
7 : [(1,3,5,2,4) : -8 -> -5]
13 : [(1,4)(2,3) : -8 -> -5]
19 : [(1,2,5,3,4) : -8 -> -5]
gap> a85 := Arrow( Hs4, (1,5,4), -8, -5 );;
gap> a85 in hs85;
true
gap> a76 in hs85;
false

gap> c3 := Subgroup( s5, [ (1,2,3) ] );; 
gap> SetName( c3, "c3" ); 
gap> c4a := Subgroup( s5, [ (1,2,3,4) ] );; 
gap> c4b := Subgroup( s5, [ (1,2,5,4) ] );; 
gap> SetName( c4a, "c4a" );  SetName( c4b, "c4b" ); 
gap> Uc3c4 := Subgroupoid( Hs4, 
>                 [ [ c3, [-9,-8,-5], [ (), (1,3,5,4), (1,2,4,5) ] ], 
>                   [ c4b, [-7,-6], [ (), (1,4,3,2,5) ] ] ] );; 
gap> Display( Uc3c4 ); 
groupoid with 2 pieces:
<     objects: [ -9, -8, -5 ]
   parent gpd: single piece groupoid with rays: < s4, [ -9, -8, -5 ], 
[ (), (4,5), (1,5) ] >
   root group: c3 = <[ (1,2,3) ]>
         rays: [ (), (1,3,5,4), (1,2,4,5) ]
<     objects: [ -7, -6 ]
   parent gpd: single piece groupoid with rays: < Group( [ (1,2,5,4), (4,5) 
 ] ), [ -7, -6 ], [ (), (2,5,3) ] >
   root group: c4b = <[ (1,2,5,4) ]>
         rays: [ (), (1,4,3,2,5) ]
gap> SetName( Uc3c4, "Uc3c4" ); 
gap> IsWideSubgroupoid( Gs5, Uc3c4 );
true
gap> Uc3 := Pieces( Uc3c4 )[1];
single piece groupoid with rays: < c3, [ -9, -8, -5 ], 
[ (), (1,3,5,4), (1,2,4,5) ] >
gap> SetName( Uc3, "Uc3" ); 

gap> hs85 := Homset( Uc3, -8, -5 ); 
<homset -8 -> -5 with head group Group( [ (2,4,3) ] )>
gap> Display( hs85 );
homset -8 -> -5 with elements:
[(1,5,3,2,4) : -8 -> -5]
[(1,5,4) : -8 -> -5]
[(1,5,2,3,4) : -8 -> -5]
gap> a85 in Uc3c4; 
true
gap> b85 := Arrow( Hs4, (1,2,4), -8, -5 );;
gap> rc85 := RightCoset( Hs4, Uc3, b85 );
<right coset of Uc3 with representative [(1,2,4) : -8 -> -5]>
gap> Display( rc85 );
right coset of Uc3 with elements:
[(1,3,5)(2,4) : -9 -> -5]
[(1,2,4) : -8 -> -5]
[(2,3,5) : -5 -> -5]
[(1,5)(2,3,4) : -9 -> -5]
[(1,2,3,5,4) : -8 -> -5]
[(2,5)(3,4) : -5 -> -5]
[(1,4,2,5) : -9 -> -5]
[(1,2,5,3,4) : -8 -> -5]
[(2,4,5) : -5 -> -5]
gap> b85 in rc85; 
true
gap> b88 := Arrow( Hs4, (1,3,5), -8, -8 );;
gap> b88 in rc85;
false
gap> lc85 := LeftCoset( Hs4, Uc3, b85 );
<left coset of Uc3 with representative [(1,2,4) : -8 -> -5]>
gap> Display( lc85 );
left coset of Uc3 with elements:
[(4,5) : -8 -> -9]
[(1,3,5) : -8 -> -8]
[(1,2,4) : -8 -> -5]
[(1,2,3)(4,5) : -8 -> -9]
[(1,2,5) : -8 -> -8]
[(1,4)(2,3) : -8 -> -5]
[(1,3,2)(4,5) : -8 -> -9]
[(1,5)(2,3) : -8 -> -8]
[(1,3,4) : -8 -> -5]
gap> b85 in lc85; 
true
gap> b88 in lc85; 
true
gap> dc85 := DoubleCoset( Hs4, Uc3, Uc3, b85 );    
<double coset of [ Uc3, Uc3 ] with representative [(1,2,4) : -8 -> -5]>
gap> Size( dc85 );
81
gap> b88 in dc85;         
'in' not yet implemented for double cosets
false

gap> a56 := ArrowNC( Hs4, 1, (1,2,3,4), -5, -6 );
[(1,2,3,4) : -5 -> -6]
gap> rc56 := RightCoset( Hs4, Uc3c4, a56 ); 
<right coset of Uc3 with representative [(1,2,3,4) : -5 -> -6]>
gap> Display( rc56 );
right coset of Uc3 with elements:
[(1,3,4,5,2) : -9 -> -6]
[(1,5,4,2) : -8 -> -6]
[(1,2,3,4) : -5 -> -6]
[(1,4,5,2,3) : -9 -> -6]
[(1,5)(2,3,4) : -8 -> -6]
[(1,2,4,3) : -5 -> -6]
[(2,4,5) : -9 -> -6]
[(1,5,3)(2,4) : -8 -> -6]
[(1,2) : -5 -> -6]
gap> lc56 := LeftCoset( Hs4, Uc3c4, a56 );
<left coset of single piece groupoid with rays: < c4b, [ -7, -6 ], 
[ (), (1,4,3,2,5) ] > with representative [(1,2,3,4) : -5 -> -6]>
gap> Display( lc56 );         
left coset of single piece groupoid with rays: < c4b, [ -7, -6 ], 
[ (), (1,4,3,2,5) ] > with elements:
[(1,3)(2,4,5) : -5 -> -7]
[(1,2,3,4) : -5 -> -6]
[(1,3,2) : -5 -> -7]
[(1,2,4,3,5) : -5 -> -6]
[(1,3,5,4) : -5 -> -7]
[(1,2,5,3) : -5 -> -6]
[(1,3,4,2,5) : -5 -> -7]
[(1,2)(4,5) : -5 -> -6]
gap> a56 in lc56;
true

gap> reps3r := RightCosetRepresentatives( Hs4, Uc3 );
[ [() : -9 -> -9], [(2,3) : -9 -> -9], [(1,3)(2,4) : -9 -> -9], 
  [(1,3,4,2) : -9 -> -9], [(1,4)(2,3) : -9 -> -9], [(1,4) : -9 -> -9], 
  [(1,2)(3,4) : -9 -> -9], [(1,2,4,3) : -9 -> -9], [() : -8 -> -8], 
  [(3,5) : -8 -> -8], [(1,3)(2,5) : -8 -> -8], [(1,3,2,5) : -8 -> -8], 
  [(1,5)(2,3) : -8 -> -8], [(1,5,2,3) : -8 -> -8], [(1,2)(3,5) : -8 -> -8], 
  [(1,2) : -8 -> -8], [() : -5 -> -5], [(3,4) : -5 -> -5], 
  [(2,4)(3,5) : -5 -> -5], [(2,4,5,3) : -5 -> -5], [(2,3)(4,5) : -5 -> -5], 
  [(2,3,5,4) : -5 -> -5], [(2,5)(3,4) : -5 -> -5], [(2,5) : -5 -> -5], 
  [() : -9 -> -7], [(2,3) : -9 -> -7], [(1,3)(2,4) : -9 -> -7], 
  [(1,3,4,2) : -9 -> -7], [(1,4)(2,3) : -9 -> -7], [(1,4) : -9 -> -7], 
  [(1,2)(3,4) : -9 -> -7], [(1,2,4,3) : -9 -> -7], [() : -9 -> -6], 
  [(2,3) : -9 -> -6], [(1,3)(2,4) : -9 -> -6], [(1,3,4,2) : -9 -> -6], 
  [(1,4)(2,3) : -9 -> -6], [(1,4) : -9 -> -6], [(1,2)(3,4) : -9 -> -6], 
  [(1,2,4,3) : -9 -> -6] ]
gap> Length( reps3r );
40
gap> rc56 := RightCoset( Hs4, Uc3, a56 );  
<right coset of Uc3 with representative [(1,2,3,4) : -5 -> -6]>
gap> Display( rc56 );
right coset of Uc3 with elements:
[(1,3,4,5,2) : -9 -> -6]
[(1,5,4,2) : -8 -> -6]
[(1,2,3,4) : -5 -> -6]
[(1,4,5,2,3) : -9 -> -6]
[(1,5)(2,3,4) : -8 -> -6]
[(1,2,4,3) : -5 -> -6]
[(2,4,5) : -9 -> -6]
[(1,5,3)(2,4) : -8 -> -6]
[(1,2) : -5 -> -6]
gap> L3 := [ ];; 
gap> for r in reps3r do 
>        c := RightCoset( Hs4, Uc3, r ); 
>        j := 0;
>        for a in c do 
>            if ( a in L3 ) then 
>                Error( "repetition!" ); 
>            else 
>                Add( L3, a ); 
>            fi; 
>            j := j+1; 
>        od;
>        if not ( j = 9 ) then 
>            Error( "length of coset not 9" ); 
>        fi;
>    od;
gap> Length( L3 );
360

gap> reps3l := LeftCosetRepresentatives( Hs4, Uc3 );
[ [() : -9 -> -9], [(2,3) : -9 -> -9], [(1,3)(2,4) : -9 -> -9], 
  [(1,2,4,3) : -9 -> -9], [(1,4)(2,3) : -9 -> -9], [(1,4) : -9 -> -9], 
  [(1,2)(3,4) : -9 -> -9], [(1,3,4,2) : -9 -> -9], [() : -8 -> -8], 
  [(3,5) : -8 -> -8], [(1,3)(2,5) : -8 -> -8], [(1,5,2,3) : -8 -> -8], 
  [(1,5)(2,3) : -8 -> -8], [(1,3,2,5) : -8 -> -8], [(1,2)(3,5) : -8 -> -8], 
  [(1,2) : -8 -> -8], [() : -5 -> -5], [(3,4) : -5 -> -5], 
  [(2,4)(3,5) : -5 -> -5], [(2,3,5,4) : -5 -> -5], [(2,3)(4,5) : -5 -> -5], 
  [(2,4,5,3) : -5 -> -5], [(2,5)(3,4) : -5 -> -5], [(2,5) : -5 -> -5], 
  [() : -7 -> -9], [(2,3) : -7 -> -9], [(1,3)(2,4) : -7 -> -9], 
  [(1,2,4,3) : -7 -> -9], [(1,4)(2,3) : -7 -> -9], [(1,4) : -7 -> -9], 
  [(1,2)(3,4) : -7 -> -9], [(1,3,4,2) : -7 -> -9], [() : -6 -> -9], 
  [(2,3) : -6 -> -9], [(1,3)(2,4) : -6 -> -9], [(1,2,4,3) : -6 -> -9], 
  [(1,4)(2,3) : -6 -> -9], [(1,4) : -6 -> -9], [(1,2)(3,4) : -6 -> -9], 
  [(1,3,4,2) : -6 -> -9] ]
gap> Length(reps3l);
40
gap> a65 := a56^-1;
[(1,4,3,2) : -6 -> -5]
gap> lc65 := LeftCoset( Hs4, Uc3, a65 );
<left coset of Uc3 with representative [(1,4,3,2) : -6 -> -5]>
gap> Display( lc65 );
left coset of Uc3 with elements:
[(1,2,5,4,3) : -6 -> -9]
[(1,2,4,5) : -6 -> -8]
[(1,4,3,2) : -6 -> -5]
[(1,3,2,5,4) : -6 -> -9]
[(1,5)(2,4,3) : -6 -> -8]
[(1,3,4,2) : -6 -> -5]
[(2,5,4) : -6 -> -9]
[(1,3,5)(2,4) : -6 -> -8]
[(1,2) : -6 -> -5]
gap> L3 := [ ];; 
gap> for r in reps3l do 
>        c := LeftCoset( Hs4, Uc3, r ); 
>        j := 0;
>        for a in c do 
>            if ( a in L3 ) then 
>                Error( "repetition!" ); 
>            else 
>                Add( L3, a ); 
>            fi; 
>            j := j+1; 
>        od;
>        if not ( j = 9 ) then 
>            Error( "length of coset not 9" ); 
>        fi;
>    od;
gap> Length( L3 );
360

gap> Uc4b := Pieces( Uc3c4 )[2];
single piece groupoid with rays: < c4b, [ -7, -6 ], [ (), (1,4,3,2,5) ] >
gap> SetName( Uc4b, "Uc4b" ); 
gap> reps4r := RightCosetRepresentatives( Hs4, Uc4b );
[ [() : -7 -> -7], [(4,5) : -7 -> -7], [(2,4) : -7 -> -7], 
  [(2,4,5) : -7 -> -7], [(2,5,4) : -7 -> -7], [(2,5) : -7 -> -7], 
  [() : -6 -> -6], [(4,5) : -6 -> -6], [(3,4) : -6 -> -6], 
  [(3,4,5) : -6 -> -6], [(3,5,4) : -6 -> -6], [(3,5) : -6 -> -6], 
  [() : -7 -> -9], [(4,5) : -7 -> -9], [(2,4) : -7 -> -9], 
  [(2,4,5) : -7 -> -9], [(2,5,4) : -7 -> -9], [(2,5) : -7 -> -9], 
  [() : -7 -> -8], [(4,5) : -7 -> -8], [(2,4) : -7 -> -8], 
  [(2,4,5) : -7 -> -8], [(2,5,4) : -7 -> -8], [(2,5) : -7 -> -8], 
  [() : -7 -> -5], [(4,5) : -7 -> -5], [(2,4) : -7 -> -5], 
  [(2,4,5) : -7 -> -5], [(2,5,4) : -7 -> -5], [(2,5) : -7 -> -5] ]
gap> Length( reps4r );
30
gap> rc65 := RightCoset( Hs4, Uc4b, a65 );  
<right coset of Uc4b with representative [(1,4,3,2) : -6 -> -5]>
gap> Display( rc65 );
right coset of Uc4b with elements:
[(1,3)(2,5,4) : -7 -> -5]
[(1,4,3,2) : -6 -> -5]
[(1,4,5,3) : -7 -> -5]
[(1,3,5,2) : -6 -> -5]
[(1,2,3) : -7 -> -5]
[(1,5,3,4,2) : -6 -> -5]
[(1,5,2,4,3) : -7 -> -5]
[(1,2)(4,5) : -6 -> -5]
gap> L4 := [ ];; 
gap> for r in reps4r do 
>        c := RightCoset( Hs4, Uc4b, r ); 
>        j := 0;
>        for a in c do 
>            if ( a in L4 ) then 
>                Error( "repetition!" ); 
>            else 
>                Add( L4, a ); 
>            fi; 
>            j := j+1; 
>        od;
>        if not ( j = 8 ) then 
>            Error( "length of coset not 8" ); 
>        fi;
>    od;
gap> Length( L4 );
240
gap> reps4l := LeftCosetRepresentatives( Hs4, Uc4b );
[ [() : -7 -> -7], [(4,5) : -7 -> -7], [(2,4) : -7 -> -7], 
  [(2,5,4) : -7 -> -7], [(2,4,5) : -7 -> -7], [(2,5) : -7 -> -7], 
  [() : -6 -> -6], [(4,5) : -6 -> -6], [(3,4) : -6 -> -6], 
  [(3,5,4) : -6 -> -6], [(3,4,5) : -6 -> -6], [(3,5) : -6 -> -6], 
  [() : -9 -> -7], [(4,5) : -9 -> -7], [(2,4) : -9 -> -7], 
  [(2,5,4) : -9 -> -7], [(2,4,5) : -9 -> -7], [(2,5) : -9 -> -7], 
  [() : -8 -> -7], [(4,5) : -8 -> -7], [(2,4) : -8 -> -7], 
  [(2,5,4) : -8 -> -7], [(2,4,5) : -8 -> -7], [(2,5) : -8 -> -7], 
  [() : -5 -> -7], [(4,5) : -5 -> -7], [(2,4) : -5 -> -7], 
  [(2,5,4) : -5 -> -7], [(2,4,5) : -5 -> -7], [(2,5) : -5 -> -7] ]
gap> Length(reps4l);
30
gap> lc56 := LeftCoset( Hs4, Uc4b, a56 );
<left coset of Uc4b with representative [(1,2,3,4) : -5 -> -6]>
gap> Display( lc56 );
left coset of Uc4b with elements:
[(1,3)(2,4,5) : -5 -> -7]
[(1,2,3,4) : -5 -> -6]
[(1,3,2) : -5 -> -7]
[(1,2,4,3,5) : -5 -> -6]
[(1,3,5,4) : -5 -> -7]
[(1,2,5,3) : -5 -> -6]
[(1,3,4,2,5) : -5 -> -7]
[(1,2)(4,5) : -5 -> -6]
gap> L4 := [ ];; 
gap> for l in reps4l do 
>        c := LeftCoset( Hs4, Uc4b, l ); 
>        j := 0;
>        for a in c do 
>            if ( a in L4 ) then 
>                Error( "repetition!" ); 
>            else 
>                Add( L4, a ); 
>            fi; 
>            j := j+1; 
>        od;
>        if not ( j = 8 ) then 
>            Error( "length of coset not 8" ); 
>        fi;
>    od;
gap> Length( L4 );
240

gap> Gs5c := Subgroupoid( Gs5, [ [s5,[-7,-6]] ] );
single piece groupoid: < s5, [ -7, -6 ] >
gap> Hs4c := Subgroupoid( Gs5c, [ [s4^(3,5),[-7,-6],[(),(2,5,3)] ] ] );
single piece groupoid with rays: < Group( [ (1,2,5,4), (4,5) ] ), [ -7, -6 ], 
[ (), (2,5,3) ] >
gap> Uc4c := Subgroupoid( Hs4c, [ [c4b,[-7,-6],[(),(1,4,3,2,5)] ] ] );
single piece groupoid with rays: < c4b, [ -7, -6 ], [ (), (1,4,3,2,5) ] >
gap> IsSubgroupoid( Hs4c, Uc4c );
true
gap> c := LeftCoset( Hs4c, Uc4b, reps4l[1] );
<left coset of Uc4b with representative [() : -7 -> -7]>
gap> reps4l7 := LeftCosetRepresentativesFromObject( Hs4c, Uc4b, -7 );
[ [() : -7 -> -7], [(4,5) : -7 -> -7], [(2,4) : -7 -> -7], 
  [(2,5,4) : -7 -> -7], [(2,4,5) : -7 -> -7], [(2,5) : -7 -> -7] ]
gap> reps4l6 := LeftCosetRepresentativesFromObject( Hs4c, Uc4b, -6 );
[ [() : -6 -> -6], [(4,5) : -6 -> -6], [(3,4) : -6 -> -6], 
  [(3,5,4) : -6 -> -6], [(3,4,5) : -6 -> -6], [(3,5) : -6 -> -6] ]

gap> dcos := DoubleCosets( s4, c3, c4a );
[ DoubleCoset(c3,(),c4a), DoubleCoset(c3,(2,3),c4a) ]
gap> List( dcos, Representative );
[ (), (2,3) ]
gap> List( dcos, Size );
[ 12, 12 ]
gap> a97 := Arrow( Hs4, (3,5), -9, -7 );;  
gap> dc1 := DoubleCoset( Hs4, Uc3, Uc4b, a97 );
<double coset of [ Uc3, Uc4b ] with representative [(3,5) : -9 -> -7]>
gap> ## a56 := Arrow( Hs4, (1,2), -5, -6 );; 
gap> dc2 := DoubleCoset( Hs4, Uc3, Uc4b, a56 );;
gap> dc1 = dc2; 
false

gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
