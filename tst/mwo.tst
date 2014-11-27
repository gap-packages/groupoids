##############################################################################
##
#W  mwo.tst                       Gpd Package                    Chris Wensley
##
##  version 1.31, 09/11/2014   
##
#Y  Copyright (C) 2000-2014, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

## SubSection 1.1.1 
gap> gpd_infolevel_saved := InfoLevel( InfoGpd );; 
gap> SetInfoLevel( InfoGpd, 0 );; 

## ========================  MAGMA  ================================ ## 
gap> tm := [[1,2,4,3],[1,2,4,3],[3,4,2,1],[3,4,2,1]];; 
gap> Display( tm );
[ [  1,  2,  4,  3 ],
  [  1,  2,  4,  3 ],
  [  3,  4,  2,  1 ],
  [  3,  4,  2,  1 ] ]
gap> m := MagmaByMultiplicationTable( tm ); 
<magma with 4 generators>
gap> SetName( m, "m" );  One(m);
fail
gap> m1 := MagmaElement(m,1);;  m2 := MagmaElement(m,2);; 
gap> m3 := MagmaElement(m,3);;  m4 := MagmaElement(m,4);; 
gap> M78 := MagmaWithObjects( m, [-8,-7] ); 
magma with objects :-
    magma = m
  objects = [ -8, -7 ]

gap> SetName( M78, "M78" ); 
gap> [ IsAssociative(M78), IsCommutative(M78), IsDomainWithObjects(M78) ]; 
[ false, false, true ]
gap> [ RootObject( M78 ), ObjectList( M78 ) ]; 
[ -8, [ -8, -7 ] ]

## SubSection 1.1.2 
gap> a78 := Arrow( M78, m2, -7, -8 ); 
[m2 : -7 -> -8]
gap> [ a78 in M78, IsArrowIn( a78, M78 ) ]; 
[ false, true ]
gap> b87 := Arrow( M78, m4, -8, -7 );;
gap> [ ElementOfArrow( b87 ), TailOfArrow( b87 ), HeadOfArrow( b87 ) ]; 
[ m4, -8, -7 ]
gap> ba := b87*a78;  ab := a78*b87;
[m4 : -8 -> -8]
[m3 : -7 -> -7]
gap> [ a78^2, ba^2, ba^3 ]; 
[ fail, [m1 : -8 -> -8], [m3 : -8 -> -8] ]
gap> [ a78*ba, ab*a78, a78*ba = ab*a78 ]; 
[ [m3 : -7 -> -8], [m4 : -7 -> -8], false ]

## SubSection 1.1.3 
gap> IsSinglePiece( M78 ); 
true
gap> IsDirectProductWithCompleteGraph( M78 );
true
gap> IsDiscrete( M78 );
false

## Section 1.2, Semigroups with objects 

## SubSection 1.2.1
gap> t := Transformation( [1,1,2,3] );; 
gap> s := Transformation( [2,2,3,3] );;
gap> r := Transformation( [2,3,4,4] );; 
gap> sgp := Semigroup( t, s, r );; 
gap> SetName( sgp, "sgp<t,s,r>" ); 
gap> S123 := SemigroupWithObjects( sgp, [-3,-2,-1] ); 
semigroup with objects :-
    magma = sgp<t,s,r>
  objects = [ -3, -2, -1 ]

gap> [ IsAssociative(S123), IsCommutative(S123) ]; 
[ true, false ]
gap> CategoriesOfObject( S123 ); 
[ "IsListOrCollection", "IsCollection", "IsExtLElement", 
  "CategoryCollections(IsExtLElement)", "IsExtRElement", 
  "CategoryCollections(IsExtRElement)", 
  "CategoryCollections(IsMultiplicativeElement)", "IsGeneralizedDomain", 
  "IsMagma", "IsDomainWithObjects", 
  "CategoryCollections(IsMultiplicativeElementWithObjects)", 
  "IsMagmaWithObjects" ]
gap> t12 := Arrow( S123, t, -1, -2 );; 
gap> s23 := Arrow( S123, s, -2, -3 );; 
gap> r31 := Arrow( S123, r, -3, -1 );; 
gap> ts13 := t12 * s23;  tsr1 := ts13 * r31;
[Transformation( [ 2, 2, 2, 3 ] ) : -1 -> -3]
[Transformation( [ 3, 3, 3 ] ) : -1 -> -1]

## Section 1.3, Monoids with objects 

## SubSection 1.3.1
gap> fm := FreeMonoid( 2, "f" );; 
gap> em := One( fm );; 
gap> gm := GeneratorsOfMonoid( fm );; 
gap> mon := fm/[ [gm[1]^3,em], [gm[1]*gm[2],gm[2]] ];; 
gap> M49 := MonoidWithObjects( mon, [-9,-4] ); 
monoid with objects :-
    magma = Monoid( [ f1, f2 ] )
  objects = [ -9, -4 ]

gap> [ IsAssociative(M49), IsCommutative(M49) ]; 
[ true, false ]
gap> ktpo := KnownTruePropertiesOfObject( M49 );; 
gap> ans := [ "CanEasilyCompareElements", "CanEasilySortElements", 
>  "IsDuplicateFree", "IsAssociative", "IsSinglePieceDomain", 
>  "IsDirectProductWithCompleteGraphDomain" ];;
gap> ForAll( ans, a -> ( a in ktpo ) ); 
true
gap> catobj := CategoriesOfObject( M49 );; 
gap> ans := [ "IsListOrCollection", "IsCollection", "IsExtLElement", 
>   "CategoryCollections(IsExtLElement)", "IsExtRElement", 
>   "CategoryCollections(IsExtRElement)", 
>   "CategoryCollections(IsMultiplicativeElement)", "IsGeneralizedDomain", 
>   "IsMagma", "IsDomainWithObjects", 
>   "CategoryCollections(IsMultiplicativeElementWithObjects)", 
>   "CategoryCollections(IsMultiplicativeElementWithObjectsAndOnes)", 
>   "IsMagmaWithObjects", "IsMagmaWithObjectsAndOnes" ];;
gap> ForAll( ans, a -> ( a in catobj ) ); 
true
gap> genM := GeneratorsOfMagmaWithOne( M49 ); 
[ [<identity ...> : -9 -> -9], [f1 : -9 -> -9], [f2 : -9 -> -9], 
  [<identity ...> : -9 -> -4], [<identity ...> : -4 -> -9] ]
gap> g2 := genM[2];; g3 := genM[3];; g4 := genM[4];; g5 := genM[5];; 
gap> g5*g3*g2*g4; 
[f2*f1 : -4 -> -4]

## Section 1.4, Structures with one or more pieces 

## SubSection 1.4.1
gap> d8 := Group( (1,2,3,4), (1,3) );; 
gap> SetName( d8, "d8" );
gap> D0 := DomainWithSingleObject( d8, 0 ); 
single piece groupoid: < d8, [ 0 ] >
gap> KnownTruePropertiesOfObject( D0 ); 
[ "CanEasilyCompareElements", "CanEasilySortElements", "IsDuplicateFree", 
  "IsAssociative", "IsSinglePieceDomain", 
  "IsDirectProductWithCompleteGraphDomain" ]
gap> p0 := Arrow( D0, (1,2,3,4), 0, 0 );; 
gap> q0 := Arrow( D0, (1,3), 0, 0 );; 
gap> p0*q0; 
[(1,2)(3,4) : 0 -> 0]
gap> Size( D0 );
8

## SubSection 1.4.2
gap> N1 := UnionOfPieces( [ M78, S123 ] ); 
magma with objects having 2 pieces :-
1: M78
2: semigroup with objects :-
    magma = sgp<t,s,r>
  objects = [ -3, -2, -1 ]

gap> ObjectList( N1 ); 
[ -8, -7, -3, -2, -1 ]
gap> N2 := UnionOfPieces( [ M49, D0 ] );  
monoid with objects having 2 pieces :-
1: monoid with objects :-
    magma = Monoid( [ f1, f2 ] )
  objects = [ -9, -4 ]
2: single piece groupoid: < d8, [ 0 ] >
gap> ObjectList( N2 ); 
[ -9, -4, 0 ]
gap> Pieces( N2 ); 
[ monoid with objects :-
        magma = Monoid( [ f1, f2 ] )
      objects = [ -9, -4 ]
    , single piece groupoid: < d8, [ 0 ] > ]
gap> N3 := UnionOfPieces( [ N1, N2] );  
magma with objects having 4 pieces :-
1: monoid with objects :-
    magma = Monoid( [ f1, f2 ] )
  objects = [ -9, -4 ]
2: M78
3: semigroup with objects :-
    magma = sgp<t,s,r>
  objects = [ -3, -2, -1 ]
4: single piece groupoid: < d8, [ 0 ] >
gap> ObjectList( N3 ); 
[ -9, -8, -7, -4, -3, -2, -1, 0 ]
gap> Length( GeneratorsOfMagmaWithObjects( N3 ) ); 
50

## this should fail since the object sets are not disjoint: 
gap> N4 := UnionOfPieces( [ S123, DomainWithSingleObject( d8, -2 ) ] );  
fail
gap> SetInfoLevel( InfoGpd, gpd_infolevel_saved );;  

#############################################################################
##
#E  mwo.tst . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
