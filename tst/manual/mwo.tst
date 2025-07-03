############################################################################
##
#W  mwo.tst                 Groupoids Package                  Chris Wensley
##

gap> START_TEST( "groupoids package: mwo.tst" );
gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

## SubSection 2.1.1 
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
gap> [ IsAssociative(M78), IsCommutative(M78) ]; 
[ false, false ]
gap> [ IsDomainWithObjects(M78), IsMagmaWithObjects(M78), IsMagma(M78) ]; 
[ true, true, false ]
gap> [ RootObject( M78 ), ObjectList( M78 ) ]; 
[ -8, [ -8, -7 ] ]

## SubSection 2.1.2 
gap> a78 := Arrow( M78, m2, -7, -8 ); 
[m2 : -7 -> -8]
gap> a78 in M78; 
true 
gap> b87 := Arrow( M78, m4, -8, -7 );;
gap> [ ElementOfArrow( b87 ), TailOfArrow( b87 ), HeadOfArrow( b87 ) ]; 
[ m4, -8, -7 ]
gap> ba := b87*a78;  ab := a78*b87;
[m4 : -8 -> -8]
[m3 : -7 -> -7]
gap> [ a78^2, ba^2, ba^3 ]; 
[ fail, [m1 : -8 -> -8], [m3 : -8 -> -8] ]
gap> ## this demonstrates non-associativity:  
gap> [ a78*ba, ab*a78, a78*ba = ab*a78 ]; 
[ [m3 : -7 -> -8], [m4 : -7 -> -8], false ]

## SubSection 2.1.3 
gap> IsSinglePiece( M78 ); 
true
gap> IsDirectProductWithCompleteDigraph( M78 );
true
gap> IsDiscreteMagmaWithObjects( M78 );
false

## Section 2.2, Semigroups with objects 

## SubSection 2.2.1
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
  "IsDomainWithObjects", 
  "CategoryCollections(IsMultiplicativeElementWithObjects)", 
  "CategoryCollections(IsMultiplicativeElementWithObjectsAndOnes)", 
  "IsMagmaWithObjects", "IsSemigroupWithObjects" ]
gap> t12 := Arrow( S123, t, -1, -2 );
[Transformation( [ 1, 1, 2, 3 ] ) : -1 -> -2]
gap> s23 := Arrow( S123, s, -2, -3 );
[Transformation( [ 2, 2, 3, 3 ] ) : -2 -> -3]
gap> r31 := Arrow( S123, r, -3, -1 );
[Transformation( [ 2, 3, 4, 4 ] ) : -3 -> -1]
gap> ts13 := t12 * s23;
[Transformation( [ 2, 2, 2, 3 ] ) : -1 -> -3]
gap> sr21 := s23 * r31;
[Transformation( [ 3, 3, 4, 4 ] ) : -2 -> -1]
gap> rt32 := r31 * t12;
[Transformation( [ 1, 2, 3, 3 ] ) : -3 -> -2]
gap> tsr1 := ts13 * r31;
[Transformation( [ 3, 3, 3 ] ) : -1 -> -1]

gap> S0 := MagmaWithSingleObject( sgp, 0 );
semigroup with objects :-
    magma = sgp<t,s,r>
  objects = [ 0 ]
gap> t0 := Arrow( S0, t, 0, 0 );            
[Transformation( [ 1, 1, 2, 3 ] ) : 0 -> 0]
gap> Size( S0 );
17

## Section 2.3, Monoids with objects 

## SubSection 2.3.1
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
gap> ans := [ "IsDuplicateFree", "IsAssociative", "IsSinglePieceDomain", 
> "IsDirectProductWithCompleteDigraphDomain" ];;  
gap> ForAll( ans, a -> ( a in ktpo ) ); 
true
gap> catobj := CategoriesOfObject( M49 );; 
gap> ans := [ "IsListOrCollection", "IsCollection", "IsExtLElement", 
>   "CategoryCollections(IsExtLElement)", "IsExtRElement", 
>   "CategoryCollections(IsExtRElement)", 
>   "CategoryCollections(IsMultiplicativeElement)", "IsGeneralizedDomain", 
>   "IsDomainWithObjects", 
>   "CategoryCollections(IsMultiplicativeElementWithObjects)", 
>   "CategoryCollections(IsMultiplicativeElementWithObjectsAndOnes)", 
>   "CategoryCollections(IsMultiplicativeElementWithObjectsAndInverses)", 
>   "IsMagmaWithObjects", "IsSemigroupWithObjects", "IsMonoidWithObjects" ];;
gap> ForAll( ans, a -> ( a in catobj ) ); 
true

## Section 2.4, Generators of magmas with objects 

gap> GeneratorsOfMagmaWithObjects( M78 );
[ [m1 : -8 -> -8], [m2 : -8 -> -8], [m3 : -8 -> -8], [m4 : -8 -> -8], 
  [m1 : -8 -> -7], [m2 : -8 -> -7], [m3 : -8 -> -7], [m4 : -8 -> -7], 
  [m1 : -7 -> -8], [m2 : -7 -> -8], [m3 : -7 -> -8], [m4 : -7 -> -8], 
  [m1 : -7 -> -7], [m2 : -7 -> -7], [m3 : -7 -> -7], [m4 : -7 -> -7] ]
gap> genS := GeneratorsOfSemigroupWithObjects( S123 );;
gap> Length( genS );                                   
27
gap> genM := GeneratorsOfMonoidWithObjects( M49 );
[ [f1 : -9 -> -9], [f2 : -9 -> -9], [<identity ...> : -9 -> -4], 
  [<identity ...> : -4 -> -9] ]
gap> g1:=genM[1];; g2:=genM[2];; g3:=genM[3];; g4:=genM[4];; 
gap> [g4,g2,g1,g3];
[ [<identity ...> : -4 -> -9], [f2 : -9 -> -9], [f1 : -9 -> -9], 
  [<identity ...> : -9 -> -4] ]
gap> g4*g2*g1*g3;
[f2*f1 : -4 -> -4]

## Section 2.5, Structures with more than one piece 

## SubSection 2.4.1
gap> N1 := UnionOfPieces( M78, S123 ); 
magma with objects having 2 pieces :-
1: M78
2: semigroup with objects :-
    magma = sgp<t,s,r>
  objects = [ -3, -2, -1 ]

gap> ObjectList( N1 ); 
[ -8, -7, -3, -2, -1 ]
gap> Pieces(N1);
[ M78, semigroup with objects :-
        magma = sgp<t,s,r>
      objects = [ -3, -2, -1 ]
     ]
gap> PieceOfObject( N1, -7 ); 
M78

gap> N2 := UnionOfPieces( M49, S0 );  
semigroup with objects having 2 pieces :-
1: monoid with objects :-
    magma = Monoid( [ f1, f2 ] )
  objects = [ -9, -4 ]
2: semigroup with objects :-
    magma = sgp<t,s,r>
  objects = [ 0 ]
gap> ObjectList( N2 ); 
[ -9, -4, 0 ]

gap> N3 := UnionOfPieces( N1, N2 );  
magma with objects having 4 pieces :-
1: monoid with objects :-
    magma = Monoid( [ f1, f2 ] )
  objects = [ -9, -4 ]
2: M78
3: semigroup with objects :-
    magma = sgp<t,s,r>
  objects = [ -3, -2, -1 ]
4: semigroup with objects :-
    magma = sgp<t,s,r>
  objects = [ 0 ]
gap> ObjectList( N3 ); 
[ -9, -8, -7, -4, -3, -2, -1, 0 ]
gap> Length( GeneratorsOfMagmaWithObjects( N3 ) ); 
50

## this should fail since the object sets are not disjoint: 
gap> N4 := UnionOfPieces( S123, MagmaWithSingleObject( sgp, -2 ) );  
fail
gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
gap> STOP_TEST( "mwo.tst", 10000 );
