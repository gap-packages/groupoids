############################################################################
##
#W  mwohom.tst              groupoids Package                   Chris Wensley
##

gap> START_TEST( "groupoids package: mwohom.tst" );
gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );;

## make mwohom.tst independent of mwo.tst 
gap> tm := [[1,2,4,3],[1,2,4,3],[3,4,2,1],[3,4,2,1]];; 
gap> m := MagmaByMultiplicationTable( tm );; 
gap> SetName( m, "m" );;
gap> m1 := MagmaElement(m,1);;  m2 := MagmaElement(m,2);; 
gap> m3 := MagmaElement(m,3);;  m4 := MagmaElement(m,4);; 
gap> M78 := MagmaWithObjects( m, [-8,-7] );; 
gap> SetName( M78, "M78" );; 
gap> a78 := Arrow( M78, m2, -7, -8 );; 
gap> b87 := Arrow( M78, m4, -8, -7 );;
gap> t := Transformation( [1,1,2,3] );; 
gap> s := Transformation( [2,2,3,3] );;
gap> r := Transformation( [2,3,4,4] );; 
gap> sgp := Semigroup( t, s, r );; 
gap> SetName( sgp, "sgp<t,s,r>" ); 
gap> S123 := SemigroupWithObjects( sgp, [-3,-2,-1] );; 
gap> t12 := Arrow( S123, t, -1, -2 );;
gap> s23 := Arrow( S123, s, -2, -3 );;
gap> r31 := Arrow( S123, r, -3, -1 );;
gap> N1 := UnionOfPieces( [ M78, S123 ] );; 

## SubSection 3.1.1
gap> tup1 := [ DirectProductElement([m1,m2]), DirectProductElement([m2,m1]), 
>              DirectProductElement([m3,m4]), DirectProductElement([m4,m3]) ];; 
gap> f1 := GeneralMappingByElements( m, m, tup1 ); 
<general mapping: m -> m >
gap> IsMagmaHomomorphism( f1 ); 
true
gap> hom1 := MagmaWithObjectsHomomorphism( M78, M78, f1, [-7,-8] ); 
magma with objects homomorphism : M78 -> M78
[ [ <mapping: m -> m >, [ -7, -8 ] ] ]
gap> [ Source( hom1 ), Range( hom1 ) ]; 
[ M78, M78 ]
gap> b87;
[m4 : -8 -> -7]
gap> im1 := ImageElm( hom1, b87 );
[m3 : -7 -> -8]
gap> i65 := IsomorphismNewObjects( M78, [-6,-5] ); 
magma with objects homomorphism : [ [ IdentityMapping( m ), [ -6, -5 ] ] ]
gap> ib87 := ImageElm( i65, b87 );
[m4 : -6 -> -5]
gap> M65 := Range( i65);; 
gap> SetName( M65, "M65" ); 
gap> j65 := InverseGeneralMapping( i65 );; 
gap> ImagesOfObjects( j65 ); 
[ -8, -7 ]
gap> comp := j65 * hom1;
magma with objects homomorphism : M65 -> M78
[ [ <mapping: m -> m >, [ -7, -8 ] ] ]
gap> ImageElm( comp, ib87 );
[m3 : -7 -> -8]

gap> M4 := UnionOfPieces( [ M78, M65 ] );;
gap> images := [ MappingToSinglePieceData( hom1 )[1], 
> MappingToSinglePieceData( j65 )[1] ]; 
[ [ <mapping: m -> m >, [ -7, -8 ] ], [ IdentityMapping( m ), [ -8, -7 ] ] ]
gap> map4 := HomomorphismToSinglePiece( M4, M78, images ); 
magma with objects homomorphism : 
[ [ <mapping: m -> m >, [ -7, -8 ] ], [ IdentityMapping( m ), [ -8, -7 ] ] ]
gap> ImageElm( map4, b87 ); 
[m3 : -7 -> -8]
gap> ImageElm( map4, ib87 );
[m4 : -8 -> -7]

## Section 3.2, Homomorphisms of semigroups and monoids with objects 
gap> t2 := Transformation( [2,2,4,1] );; 
gap> s2 := Transformation( [1,1,4,4] );;
gap> r2 := Transformation( [4,1,3,3] );; 
gap> sgp2 := Semigroup( [ t2, s2, r2 ] );;
gap> SetName( sgp2, "sgp<t2,s2,r2>" );
gap> ##  apparently no method for transformation semigroups available for: 
gap> ##  nat := NaturalHomomorphismByGenerators( sgp, sgp2 );  so we use: 
gap> ##  in the function flip below t is a transformation on [1..n] 
gap> flip := function( t ) 
>     local i, j, k, L, L1, L2, n; 
>     n := DegreeOfTransformation( t );  
>     L := ImageListOfTransformation( t ); 
>     if IsOddInt( n ) then 
>         n := n+1; 
>         L1 := Concatenation( L, [n] );
>     else 
>         L1 := L; 
>     fi;
>     L2 := ShallowCopy( L1 );
>     for i in [1..n] do 
>         if IsOddInt(i) then j:=i+1; else j:=i-1; fi; 
>         k := L1[j]; 
>         if IsOddInt(k) then L2[i]:=k+1; else L2[i]:=k-1; fi; 
>     od; 
>     return( Transformation( L2 ) ); 
> end;; 
gap> smap := MappingByFunction( sgp, sgp2, flip );; 
gap> ok := RespectsMultiplication( smap ); 
true
gap> [ t, Image( smap, t ) ]; 
[ Transformation( [ 1, 1, 2, 3 ] ), Transformation( [ 2, 2, 4, 1 ] ) ]
gap> [ s, Image( smap, s ) ]; 
[ Transformation( [ 2, 2, 3, 3 ] ), Transformation( [ 1, 1, 4, 4 ] ) ]
gap> [ r, Image( smap, r ) ]; 
[ Transformation( [ 2, 3, 4, 4 ] ), Transformation( [ 4, 1, 3, 3 ] ) ]
gap> SetName( smap, "smap" ); 
gap> T123 := SemigroupWithObjects( sgp2, [-13,-12,-11] );; 
gap> shom := MagmaWithObjectsHomomorphism( S123, T123, smap, [-11,-12,-13] );; 
gap> it12 := ImageElm( shom, t12 );;  [ t12, it12 ]; 
[ [Transformation( [ 1, 1, 2, 3 ] ) : -1 -> -2], 
  [Transformation( [ 2, 2, 4, 1 ] ) : -13 -> -12] ]
gap> is23 := ImageElm( shom, s23 );;  [ s23, is23 ]; 
[ [Transformation( [ 2, 2, 3, 3 ] ) : -2 -> -3], 
  [Transformation( [ 1, 1, 4, 4 ] ) : -12 -> -11] ]
gap> ir31 := ImageElm( shom, r31 );;  [ r31, ir31 ]; 
[ [Transformation( [ 2, 3, 4, 4 ] ) : -3 -> -1], 
  [Transformation( [ 4, 1, 3, 3 ] ) : -11 -> -13] ]

## Section 3.3, Homomorphisms from several pieces

## SubSection 3.3.1
gap> N4 := UnionOfPieces( [ M78, T123 ] );; 
gap> h14 := HomomorphismByUnionNC( N1, N4, [ hom1, shom ] ); 
magma with objects homomorphism : 
[ magma with objects homomorphism : M78 -> M78
    [ [ <mapping: m -> m >, [ -7, -8 ] ] ], magma with objects homomorphism : 
    [ [ smap, [ -11, -12, -13 ] ] ] ]
gap> ImageElm( h14, a78 );
[m1 : -8 -> -7]
gap> ImageElm( h14, r31 );
[Transformation( [ 4, 1, 3, 3 ] ) : -11 -> -13]

## SubSection 3.3.2
gap> IsInjectiveOnObjects( h14 );
true
gap> IsSurjectiveOnObjects( h14 );
true
gap> IsBijectiveOnObjects( h14 ); 
true
gap> IsEndomorphismWithObjects( h14 ); 
false
gap> IsAutomorphismWithObjects( h14 ); 
false

## SubSection 3.3.2
gap> swap := function(a) return Arrow(M78,a![2],a![4],a![3]); end;      
function( a ) ... end
gap> swapmap := MappingWithObjectsByFunction( M78, M78, swap, [-7,-8] );
magma with objects mapping by function : M78 -> M78
function: function ( a )
    return Arrow( M78, a![2], a![4], a![3] );
end

gap> a78; ImageElm( swapmap, a78 );              
[m2 : -7 -> -8]
[m2 : -8 -> -7]
gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
gap> STOP_TEST( "mwohom.tst", 10000 );
