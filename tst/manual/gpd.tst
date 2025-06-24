############################################################################
##
#W  gpd.tst                 groupoids Package                  Chris Wensley
##

gap> START_TEST( "groupoids package: gpd.tst" );
gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

## SubSection 4.1.1 
gap> s4 := Group( (1,2,3,4), (3,4) );; 
gap> d8 := Subgroup( s4, [ (1,2,3,4), (1,3) ] );;
gap> SetName( s4, "s4" );  SetName( d8, "d8" ); 
gap> Gs4 := SinglePieceGroupoid( s4, [-15 .. -11] ); 
single piece groupoid: < s4, [ -15 .. -11 ] >
gap> Gd8 := Groupoid( d8, [-9,-8,-7] );
single piece groupoid: < d8, [ -9, -8, -7 ] >
gap> c6 := Group( (5,6,7)(8,9) );;
gap> SetName( c6, "c6" );
gap> Gc6 := MagmaWithSingleObject( c6, -10 );
single piece groupoid: < c6, [ -10 ] >
gap> SetName( Gs4, "Gs4" );  SetName( Gd8, "Gd8" );  SetName( Gc6, "Gc6" );  

## SubSection 4.1.2 
gap> ObjectList( Gs4 );
[ -15 .. -11 ] 
gap> f2 := FreeGroup(2);;
gap> Gf2c6 := Groupoid( c6, GeneratorsOfGroup(f2) );
single piece groupoid: < c6, [ f1, f2 ] >
gap> Arrow( Gf2c6, (5,7,6), f2.1, f2.2 );
[(5,7,6) : f1 -> f2]
gap> Gabc := Groupoid( d8, [ "a", "b", "c" ] );
single piece groupoid: < d8, [ "a", "b", "c" ] >
gap> Arrow( Gabc, (2,4), "c", "b" );
[(2,4) : c -> b]

## SubSection 4.1.3
gap> f2 := FreeGroup( 2 );;
gap> Gf2 := Groupoid( f2, -20 );;
gap> SetName( f2, "f2" );  SetName( Gf2, "Gf2" ); 
gap> q8 := QuaternionGroup( 8 );;
gap> genq8 := GeneratorsOfGroup( q8 );;
gap> x := genq8[1];;  y := genq8[2];;
gap> Gq8 := Groupoid( q8, [ -18, -17 ] );;
gap> SetName( q8, "q8" );  SetName( Gq8, "Gq8" );
gap> gl43 := SpecialLinearGroup( 4, 3 );;
gap> Ggl43 := SinglePieceGroupoid( gl43, [ -23..-21 ] );;
gap> SetName( gl43, "gl43" );  SetName( Ggl43, "Ggl43" );
gap> [ IsMatrixGroupoid( Ggl43 ), IsFpGroupoid( Gf2 ), IsFreeGroupoid( Gf2 ),
>      IsPcGroupoid( Gq8 ), IsPermGroupoid( Gs4 ) ]; 
[ true, true, true, true, true ]

## SubSection 4.1.4
gap> U3 := UnionOfPieces( [ Gs4, Gc6, Gd8 ] );;
gap> Display( U3 );
groupoid with 3 pieces:
< objects: [ -15 .. -11 ]
    group: s4 = <[ (1,2,3,4), (3,4) ]> >
< objects: [ -10 ]
    group: c6 = <[ (5,6,7)(8,9) ]> >
< objects: [ -9, -8, -7 ]
    group: d8 = <[ (1,2,3,4), (1,3) ]> >
gap> Pieces( U3 );
[ Gs4, Gc6, Gd8 ]
gap> ObjectList( U3 );
[ -15, -14, -13, -12, -11, -10, -9, -8, -7 ]
gap> [ Size(Gs4), Size(Gd8), Size(Gc6), Size(U3) ];
[ 600, 72, 6, 678 ]
gap> U2 := Groupoid( [ Gf2, Gq8 ] );;
gap> [ Size(Gf2), Size(Gq8), Size(U2) ];           
[ infinity, 32, infinity ]
gap> U5 := UnionOfPieces( [ U3, U2 ] );
groupoid with 5 pieces:
[ Gf2, Gq8, Gs4, Gc6, Gd8 ]
gap> Display( U5 );
groupoid with 5 pieces:
< objects: [ -20 ]
    group: f2 = <[ f1, f2 ]> >
< objects: [ -18, -17 ]
    group: q8 = <[ x, y, y2 ]> >
< objects: [ -15 .. -11 ]
    group: s4 = <[ (1,2,3,4), (3,4) ]> >
< objects: [ -10 ]
    group: c6 = <[ (5,6,7)(8,9) ]> >
< objects: [ -9, -8, -7 ]
    group: d8 = <[ (1,2,3,4), (1,3) ]> >
gap> V3 := ReplaceOnePieceInUnion( U3, Gd8, Gq8 ); 
groupoid with 3 pieces:
[ Gq8, Gs4, Gc6 ]
gap> ObjectList( V3 );             
[ -18, -17, -15, -14, -13, -12, -11, -10 ]
gap> V2 := ReplaceOnePieceInUnion( U2, 2, Gd8 ); 
groupoid with 2 pieces:
[ Gf2, Gd8 ]

## SubSection 4.1.5
gap> HGd8 := HomogeneousGroupoid( Gd8, 
>                [ [-39,-38,-37], [-36,-35,-34], [-33,-32,-31] ] );
homogeneous groupoid with 3 pieces:
1:  single piece groupoid: < d8, [ -39, -38, -37 ] >
2:  single piece groupoid: < d8, [ -36, -35, -34 ] >
3:  single piece groupoid: < d8, [ -33, -32, -31 ] >
gap> Size( HGd8 );   ## 8x3x3 + 8x3x3 + 8x3x3
216
gap> PieceIsomorphisms( HGd8 );
[ groupoid homomorphism : 
    [ [ [(1,2,3,4) : -39 -> -39], [(1,3) : -39 -> -39], [() : -39 -> -38], 
          [() : -39 -> -37] ], 
      [ [(1,2,3,4) : -36 -> -36], [(1,3) : -36 -> -36], [() : -36 -> -35], 
          [() : -36 -> -34] ] ], groupoid homomorphism : 
    [ [ [(1,2,3,4) : -39 -> -39], [(1,3) : -39 -> -39], [() : -39 -> -38], 
          [() : -39 -> -37] ], 
      [ [(1,2,3,4) : -33 -> -33], [(1,3) : -33 -> -33], [() : -33 -> -32], 
          [() : -33 -> -31] ] ] ]
gap> HDc6 := HomogeneousDiscreteGroupoid( c6, [-27..-24] ); 
homogeneous, discrete groupoid: < c6, [ -27 .. -24 ] >
gap> Size( HDc6 );   ## 6x4
24
gap> RepresentationsOfObject( Gd8 );
[ "IsComponentObjectRep", "IsAttributeStoringRep", "IsMWOSinglePieceRep" ]
gap> RepresentationsOfObject( HGd8 );
[ "IsComponentObjectRep", "IsAttributeStoringRep", "IsPiecesRep" ]
gap> RepresentationsOfObject( HDc6 );
[ "IsComponentObjectRep", "IsAttributeStoringRep", 
  "IsHomogeneousDiscreteGroupoidRep" ]
gap> ktpo := KnownTruePropertiesOfObject( HDc6 );; 
gap> ans := 
> [ "IsDuplicateFree", "IsAssociative", "IsCommutative", 
>   "IsDiscreteDomainWithObjects", "IsHomogeneousDomainWithObjects" ];;
gap> ForAll( ans, a -> ( a in ktpo ) ); 
true

## SubSection 4.1.6
gap> prod := DirectProductOp( [Gd8,Gc6], Gd8 );
single piece groupoid: < Group( [ (1,2,3,4), (1,3), (5,6,7)(8,9) ] ), 
[ [ -9, -10 ], [ -8, -10 ], [ -7, -10 ] ] >
gap> Projection( prod, 1 );                    
groupoid homomorphism : 
[ [ [(1,2,3,4) : [ -9, -10 ] -> [ -9, -10 ]], [(1,3) : [ -9, -10 ] -> [ -9, -10 ]]
        , [(5,6,7)(8,9) : [ -9, -10 ] -> [ -9, -10 ]], 
      [() : [ -9, -10 ] -> [ -8, -10 ]], [() : [ -9, -10 ] -> [ -7, -10 ]] ], 
  [ [(1,2,3,4) : -9 -> -9], [(1,3) : -9 -> -9], [() : -9 -> -9], 
      [() : -9 -> -8], [() : -9 -> -7] ] ]
gap> Embedding( prod, 2 );                        
groupoid homomorphism : 
[ [ [(5,6,7)(8,9) : -10 -> -10] ], 
  [ [(5,6,7)(8,9) : [ -9, -10 ] -> [ -9, -10 ]] ] ]
gap> DirectProductInfo( prod );
rec( embeddings := [ , groupoid homomorphism : 
        [ [ [(5,6,7)(8,9) : -10 -> -10] ], 
          [ [(5,6,7)(8,9) : [ -9, -10 ] -> [ -9, -10 ]] ] ] ], first := Gd8, 
  groupoids := [ Gd8, Gc6 ], groups := [ d8, c6 ], 
  objectlists := [ [ -9, -8, -7 ], [ -10 ] ], 
  projections := [ groupoid homomorphism : 
        [ [ [(1,2,3,4) : [ -9, -10 ] -> [ -9, -10 ]], 
              [(1,3) : [ -9, -10 ] -> [ -9, -10 ]], 
              [(5,6,7)(8,9) : [ -9, -10 ] -> [ -9, -10 ]], 
              [() : [ -9, -10 ] -> [ -8, -10 ]], 
              [() : [ -9, -10 ] -> [ -7, -10 ]] ], 
          [ [(1,2,3,4) : -9 -> -9], [(1,3) : -9 -> -9], [() : -9 -> -9], 
              [() : -9 -> -8], [() : -9 -> -7] ] ] ] )

## Section 4.2 : Groupoid elements: stars; costars; homsets ###

## SubSection 4.2.1
gap> e1 := GroupoidElement( Gd8, (1,2,3,4), -9, -8 );
[(1,2,3,4) : -9 -> -8]
gap> e2 := Arrow( Gd8, (1,3), -8, -7 );
[(1,3) : -8 -> -7]
gap> Print( [ ElementOfArrow(e1), TailOfArrow(e1), HeadOfArrow(e1) ], "\n" );
[ (1,2,3,4), -9, -8 ]
gap> e1e2 := e1*e2;
[(1,2)(3,4) : -9 -> -7]
gap> e2*e1;
fail
gap> e3 := Arrow( Gd8, (2,4), -7, -9 );;
gap> loop := e1e2*e3;
[(1,4,3,2) : -9 -> -9]
gap> loop^2;
[(1,3)(2,4) : -9 -> -9]

## SubSection 4.2.2
gap> i8 := IdentityArrow( Gd8, -8 );
[() : -8 -> -8]
gap> [ e1*i8, i8*e1, e1^-1]; 
[ [(1,2,3,4) : -9 -> -8], fail, [(1,4,3,2) : -8 -> -9] ]

## SubSection 4.2.3
gap> [ i8, loop ]; 
[ [() : -8 -> -8], [(1,4,3,2) : -9 -> -9] ]
gap> [ Order( i8 ), Order(loop) ];
[ 1, 4 ]

## SubSection 4.2.4
gap> star9 := ObjectStar( Gd8, -9 );
<star at -9 with vertex group d8>
gap> Size( star9 ); 
24
gap> ## print the elements in star9 from 19 to 24
gap> iter := Iterator( star9 );;              
gap> for i in [1..18] do a := NextIterator( iter ); od; 
gap> for i in [19..24] do Print( i, " : ", NextIterator( iter ), "\n" ); od; 
19 : [(1,2,3,4) : -9 -> -9]
20 : [(1,2,3,4) : -9 -> -8]
21 : [(1,2,3,4) : -9 -> -7]
22 : [(1,2)(3,4) : -9 -> -9]
23 : [(1,2)(3,4) : -9 -> -8]
24 : [(1,2)(3,4) : -9 -> -7]
gap> costar12 := ObjectCostar( Gs4, -12 );
<costar at -12 with vertex group s4>
gap> Size( costar12 );
120
gap> Elements( q8 );
[ <identity> of ..., x, y, y2, x*y, x*y2, y*y2, x*y*y2 ]
gap> hsetq8 := Homset( Gq8, -18, -17 );
<homset -18 -> -17 with head group q8>
gap> Perform( hsetq8, Display );
[<identity> of ... : -18 -> -17]
[x : -18 -> -17]
[y : -18 -> -17]
[y2 : -18 -> -17]
[x*y : -18 -> -17]
[x*y2 : -18 -> -17]
[y*y2 : -18 -> -17]
[x*y*y2 : -18 -> -17]

### Section 4.3 : Subgroupoids ###

## SubSection 4.3.2 : SubgroupoidWithRays
gap> Kd8 := SubgroupoidWithRays( Gs4, d8, 
>               [ (), (1,2,3), (1,2,4), (1,3,4), (2,3,4) ] );
single piece groupoid with rays: < d8, [ -15 .. -11 ], 
[ (), (1,2,3), (1,2,4), (1,3,4), (2,3,4) ] >
gap> SetName( Kd8, "Kd8" );
gap> IsSubgroupoid( Gs4, Kd8 );
true
gap> IsWideSubgroupoid( Gs4, Kd8 );
true
gap> RaysOfGroupoid( Kd8 );       
[ (), (1,2,3), (1,2,4), (1,3,4), (2,3,4) ]
gap> RayArrowsOfGroupoid( Kd8 );  
[ [() : -15 -> -15], [(1,2,3) : -15 -> -14], [(1,2,4) : -15 -> -13],
  [(1,3,4) : -15 -> -12], [(2,3,4) : -15 -> -11] ]
gap> IsDirectProductWithCompleteDigraph( Kd8 );
false
gap> ObjectGroup( Kd8, -14 );
Group([ (1,4,2,3), (1,2) ])

## SubSection 4.3.3
gap> s3 := Subgroup( s4, [ (1,2,3), (2,3) ] );; 
gap> SetName( s3, "s3" ); 
gap> Hs4 := SubgroupoidByObjects( Gs4, [-14,-13,-12] );  
single piece groupoid: < s4, [ -14, -13, -12 ] >
gap> Hs3 := SubgroupoidBySubgroup( Hs4, s3 );
single piece groupoid: < s3, [ -14, -13, -12 ] >

## SubSection 4.3.4 : SubgroupoidByPieces 
gap> Display( Gd8 );
single piece groupoid: Gd8
  objects: [ -9, -8, -7 ]
    group: d8 = <[ (1,2,3,4), (1,3) ]>
gap> c4 := Subgroup( d8, [ (1,2,3,4) ] );;
gap> k4 := Subgroup( d8, [ (1,3), (2,4) ] );;
gap> SetName( c4, "c4" );  SetName( k4, "k4" );
gap> Jd8 := Subgroupoid( Gd8, [ [ k4, [-9] ], [ c4, [-8,-7] ] ] );;
gap> SetName( Jd8, "Jd8" );
gap> Display( Jd8 );
groupoid with 2 pieces:
< objects: [ -9 ]
    group: k4 = <[ (1,3), (2,4) ]> >
< objects: [ -8, -7 ]
    group: c4 = <[ (1,2,3,4) ]> >
gap> [ Parent( Jd8 ), IsWideSubgroupoid( Gd8, Jd8 ) ]; 
[ Gd8, true ]
gap> U2;
groupoid with 2 pieces:
[ Gf2, Gq8 ]
gap> genf2b := List( GeneratorsOfGroup(f2), g -> g^2 );
[ f1^2, f2^2 ]
gap> f2b := Subgroup( f2, genf2b );;
gap> JU2 := SubgroupoidByPieces( U2, [ [q8,[-17]], [f2b,[-20]] ] );
groupoid with 2 pieces:
1:  single piece groupoid: < Group( [ f1^2, f2^2 ] ), [ -20 ] >
2:  single piece groupoid: < q8, [ -17 ] >
gap> [ IsWideSubgroupoid(U2,JU2), IsSubgroupoid(Gf2,Groupoid(f2b,[-20])) ];
[ false, true ]

## SubSection 4.3.5
gap> FullTrivialSubgroupoid( Jd8 );
groupoid with 2 pieces:
1:  single piece groupoid: < id(k4), [ -9 ] >
2:  single piece groupoid: < id(c4), [ -8, -7 ] >
gap> DiscreteTrivialSubgroupoid( Jd8 );
groupoid with 3 pieces:
1:  single piece groupoid: < id(k4), [ -9 ] >
2:  single piece groupoid: < id(c4), [ -8 ] >
3:  single piece groupoid: < id(c4), [ -7 ] >

## SubSection 4.3.6
gap> U3;
groupoid with 3 pieces:
[ Gs4, Gc6, Gd8 ]
gap> DiscreteSubgroupoid( U3, [ s3, s3^(2,4), c4, k4 ], [-15,-13,-9,-7] );
groupoid with 4 pieces:
1:  single piece groupoid: < s3, [ -15 ] >
2:  single piece groupoid: < Group( [ (1,4,3), (3,4) ] ), [ -13 ] >
3:  single piece groupoid: < c4, [ -9 ] >
4:  single piece groupoid: < k4, [ -7 ] >
gap> MaximalDiscreteSubgroupoid( Jd8 );
groupoid with 3 pieces:
1:  single piece groupoid: < k4, [ -9 ] >
2:  single piece groupoid: < c4, [ -8 ] >
3:  single piece groupoid: < c4, [ -7 ] >

## SubSection 4.3.7 
gap> a1 := Arrow( Kd8, (1,2,3,4), -15, -15 );;
gap> a2 := Arrow( Kd8, (1,2), -15, -13 );;
gap> a3 := Arrow( Kd8, (3,4), -15, -11 );;
gap> SinglePieceSubgroupoidByGenerators( Kd8, [a1,a2,a3] );
single piece groupoid with rays: < Group( [ (1,2,3,4) ] ), [ -15, -13, -11 ], 
[ (), (1,2), (3,4) ] >

### Section 4.4 : Left, Right and Double Cosets ###

## SubSection 4.4.1
gap> e2;                            
[(1,3) : -8 -> -7]
gap> re2 := RightCoset( Gd8, Jd8, e2 );
<right coset of single piece groupoid: < c4, [ -8, -7 ] > 
with representative [(1,3) : -8 -> -7]>
gap> Perform( re2, Display );
[(1,3) : -8 -> -7]
[(1,3) : -7 -> -7]
[(2,4) : -8 -> -7]
[(2,4) : -7 -> -7]
[(1,4)(2,3) : -8 -> -7]
[(1,4)(2,3) : -7 -> -7]
[(1,2)(3,4) : -8 -> -7]
[(1,2)(3,4) : -7 -> -7]
gap> rcrd8 := RightCosetRepresentatives( Gd8, Jd8 );
[ [() : -9 -> -9], [(1,4,3,2) : -9 -> -9], [() : -9 -> -8], 
  [(1,4,3,2) : -9 -> -8], [() : -9 -> -7], [(1,4,3,2) : -9 -> -7], 
  [() : -8 -> -8], [(2,4) : -8 -> -8], [() : -7 -> -7], [(2,4) : -7 -> -7], 
  [() : -8 -> -9], [(2,4) : -8 -> -9] ]
gap> le2 := LeftCoset( Gd8, Jd8, e2 ); 
<left coset of single piece groupoid: < c4, [ -8, -7 ] > with representative [
(1,3) : -8 -> -8]>
gap> Perform( le2, Display );
[(1,3) : -8 -> -8]
[(1,3) : -8 -> -7]
[(2,4) : -8 -> -8]
[(2,4) : -8 -> -7]
[(1,4)(2,3) : -8 -> -8]
[(1,4)(2,3) : -8 -> -7]
[(1,2)(3,4) : -8 -> -8]
[(1,2)(3,4) : -8 -> -7]
gap> lcrd8 := LeftCosetRepresentatives( Gd8, Jd8 );
[ [() : -9 -> -9], [(1,2,3,4) : -9 -> -9], [() : -8 -> -9], 
  [(1,2,3,4) : -8 -> -9], [() : -7 -> -9], [(1,2,3,4) : -7 -> -9], 
  [() : -8 -> -8], [(2,4) : -8 -> -8], [() : -7 -> -7], [(2,4) : -7 -> -7], 
  [() : -9 -> -8], [(2,4) : -9 -> -8] ]
gap> lcr7 := LeftCosetRepresentativesFromObject( Gd8, Jd8, -7 );
[ [() : -7 -> -9], [(1,2,3,4) : -7 -> -9], [() : -7 -> -7], 
  [(2,4) : -7 -> -7] ]
gap> de2 := DoubleCoset( Gd8, Jd8, Jd8, e2 );
<double coset of [ single piece groupoid: < c4, [ -8, -7 ] >, 
  single piece groupoid: < c4, [ -8, -7 ] > ] with representative [(1,3) : 
-8 -> -8]>
gap> Perform( de2, Display );
[(2,4) : -8 -> -8]
[(2,4) : -8 -> -7]
[(2,4) : -7 -> -8]
[(2,4) : -7 -> -7]
[(1,3) : -8 -> -8]
[(1,3) : -8 -> -7]
[(1,3) : -7 -> -8]
[(1,3) : -7 -> -7]
[(1,2)(3,4) : -8 -> -8]
[(1,2)(3,4) : -8 -> -7]
[(1,2)(3,4) : -7 -> -8]
[(1,2)(3,4) : -7 -> -7]
[(1,4)(2,3) : -8 -> -8]
[(1,4)(2,3) : -8 -> -7]
[(1,4)(2,3) : -7 -> -8]
[(1,4)(2,3) : -7 -> -7]
gap> dcrd8 := DoubleCosetRepresentatives( Gd8, Jd8, Jd8 );
[ [() : -9 -> -9], [(1,4,3,2) : -9 -> -9], [() : -9 -> -8], [() : -8 -> -9], 
  [() : -8 -> -8], [(2,4) : -8 -> -8] ]

### Section 4.5 : conjugation ###

## SubSection 4.5.1
gap> p := Arrow( Gd8, (1,3), -9, -9 );; 
gap> q := Arrow( Gd8, (1,2,3,4), -8, -9 );; 
gap> r := Arrow( Gd8, (1,2)(3,4), -9, -7 );; 
gap> s := Arrow( Gd8, (1,2,3,4), -7, -8 );; 
gap> ##  conjugation with elements p, q, and r in Gd8: 
gap> p^q;
[(2,4) : -8 -> -8]
gap> p^r;
[(2,4) : -7 -> -7]
gap> q^p;
[() : -8 -> -9]
gap> q^r;
[(2,4) : -8 -> -7]
gap> r^p;
[(1,4,3,2) : -9 -> -7]
gap> r^q;
[(2,4) : -8 -> -7]
gap> s^p;                           
[(1,2,3,4) : -7 -> -8]
gap> s^q;
[(1,3)(2,4) : -7 -> -9]
gap> s^r;
[(1,3) : -9 -> -8]

## SubSection 4.5.2
gap> a := Arrow( Gs4, (1,2), -13, -12 ); 
[(1,2) : -13 -> -12]
gap> ConjugateGroupoid( Kd8, a );
single piece groupoid with rays: < Group( [ (1,2,3,4), (1,3) ] ), 
[ -15, -14, -13, -12, -11 ], [ (), (1,2,3), (1,3,4,2), (2,4), (2,3,4) ] >

## SubSection 4.6.1
gap> s3a := Group( (1,2), (2,3) );; 
gap> s3b := Group( (4,6,8)(5,7,9), (4,9)(5,8)(6,7) );;
gap> s3c := Group( (4,6,8)(5,7,9), (5,9)(6,8) );;
gap> SetName( s3a, "s3a" );;
gap> SetName( s3b, "s3b" );;
gap> SetName( s3c, "s3c" );; 
gap> ida := IdentityMapping( s3a );; 
gap> isoab := IsomorphismGroups( s3a, s3b );; 
gap> isoac := IsomorphismGroups( s3a, s3c );;
gap> isos1 := [ ida, isoab, isoac ];; 
gap> G1 := GroupoidByIsomorphisms( s3a, [-3,-2,-1], isos1 );; 
gap> gens1 := GeneratorsOfGroupoid( G1 );                    
[ [[ (1,2), (1,2) ] : -3 -> -3], [[ (2,3), (2,3) ] : -3 -> -3], 
  [[ (), () ] : -3 -> -2], [[ (), () ] : -3 -> -1] ]
gap> x1 := ImageElm( isos1[2], (1,2) );;
gap> a1 := Arrow( G1, [ (1,2), x1 ], -3, -2 );
[[ (1,2), (4,5)(6,9)(7,8) ] : -3 -> -2]
gap> a1^-1;
[[ (4,5)(6,9)(7,8), (1,2) ] : -2 -> -3]
gap> y1 := ImageElm( isos1[2], (2,3) );;
gap> z1 := ImageElm( isos1[3], (2,3) );;
gap> b1 := Arrow( G1, [ y1, z1 ], -2, -1 );
[[ (4,9)(5,8)(6,7), (5,9)(6,8) ] : -2 -> -1]
gap> c1 := a1*b1;
[[ (1,3,2), (4,8,6)(5,9,7) ] : -3 -> -1]

gap> isopc := IsomorphismPcGroup( s3a );; 
gap> s3p := Image( isopc );;
gap> f2 := FreeGroup( 2 );; 
gap> s3f := f2/[ f2.1^3, f2.2^2, (f2.1*f2.2)^2 ];; 
gap> isofp := GroupHomomorphismByImages(s3a,s3f,[(1,2,3),(2,3)],[s3f.1,s3f.2]);;
gap> isos2 := [ ida, isopc, isofp ];;
gap> G2 := GroupoidByIsomorphisms( s3a, [-6,-5,-4], isos2 );; 
gap> gens2 := GeneratorsOfGroupoid( G2 );
[ [[ (1,2), (1,2) ] : -6 -> -6], [[ (2,3), (2,3) ] : -6 -> -6], 
  [[ (), <identity> of ... ] : -6 -> -5], [[ (), <identity ...> ] : -6 -> -4] 
 ]
gap> x2 := ImageElm( isos2[2], (1,2) );;
gap> a2 := Arrow( G2, [ (1,2), x2 ], -6, -5 );
[[ (1,2), f1*f2 ] : -6 -> -5]
gap> a2^-1;
[[ f1*f2, (1,2) ] : -5 -> -6]
gap> y2 := ImageElm( isos2[2], (2,3) );;
gap> z2 := ImageElm( isos2[3], (2,3) );;
gap> b2 := Arrow( G2, [ y2, z2 ], -5, -4 );
[[ f1, f2^-1 ] : -5 -> -4]
gap> c2 := a2*b2; 
[[ (1,3,2), f1^2 ] : -6 -> -4]

## SubSection 4.7.1
gap> d8 := Group( (1,2,3,4), (1,3) );; 
gap> ed8 := Elements( d8 );; 
gap> rd8 := SinglePieceGroupoidWithRays( d8, ed8, ed8 );
single piece groupoid with rays: < Group( [ (1,2,3,4), (1,3) ] ), 
[ (), (2,4), (1,2)(3,4), (1,2,3,4), (1,3), (1,3)(2,4), (1,4,3,2), (1,4)(2,3) 
 ], [ (), (2,4), (1,2)(3,4), (1,2,3,4), (1,3), (1,3)(2,4), (1,4,3,2), 
  (1,4)(2,3) ] >
gap> Homset( rd8, (2,4), (1,3) );
<homset (2,4) -> (1,3) with head group Group( [ (1,4,3,2), (1,3) ] )>
gap> Display( last ); 
<homset (2,4) -> (1,3) with elements:
[(1,3)(2,4) : (2,4) -> (1,3)]
[(1,3) : (2,4) -> (1,3)]
[() : (2,4) -> (1,3)]
[(2,4) : (2,4) -> (1,3)]
[(1,4,3,2) : (2,4) -> (1,3)]
[(1,4)(2,3) : (2,4) -> (1,3)]
[(1,2,3,4) : (2,4) -> (1,3)]
[(1,2)(3,4) : (2,4) -> (1,3)]

## SubSection 4.7.2
gap> M := Monoid( Transformation( [1,1,2,3] ), Transformation( [1,2,4,3] ) );
<transformation monoid of degree 4 with 2 generators>
gap> rag := RightActionGroupoid( M );
groupoid with 8 pieces:
1:  single piece groupoid with rays: < Group( 
[ IdentityTransformation, Transformation( [ 1, 2, 4, 3 ] ) ] ), 
[ Transformation( [ 1, 1, 1, 1 ] ) ], [ IdentityTransformation ] >
2:  single piece groupoid with rays: < Group( 
[ IdentityTransformation, Transformation( [ 1, 2, 4, 3 ] ) ] ), 
[ Transformation( [ 1, 1, 1, 2 ] ) ], [ IdentityTransformation ] >
3:  single piece groupoid with rays: < Group( [ IdentityTransformation ] ), 
[ Transformation( [ 1, 1, 1, 3 ] ), Transformation( [ 1, 1, 1 ] ) ], 
[ IdentityTransformation, Transformation( [ 1, 2, 4, 3 ] ) ] >
4:  single piece groupoid with rays: < Group( 
[ IdentityTransformation, Transformation( [ 1, 2, 4, 3 ] ) ] ), 
[ Transformation( [ 1, 1, 2, 1 ] ) ], [ IdentityTransformation ] >
5:  single piece groupoid with rays: < Group( [ IdentityTransformation ] ), 
[ Transformation( [ 1, 1, 2, 3 ] ), Transformation( [ 1, 1, 2 ] ) ], 
[ IdentityTransformation, Transformation( [ 1, 2, 4, 3 ] ) ] >
6:  single piece groupoid with rays: < Group( [ IdentityTransformation ] ), 
[ Transformation( [ 1, 1, 3, 1 ] ), Transformation( [ 1, 1, 4, 1 ] ) ], 
[ IdentityTransformation, Transformation( [ 1, 2, 4, 3 ] ) ] >
7:  single piece groupoid with rays: < Group( [ IdentityTransformation ] ), 
[ Transformation( [ 1, 1, 3, 2 ] ), Transformation( [ 1, 1, 4, 2 ] ) ], 
[ IdentityTransformation, Transformation( [ 1, 2, 4, 3 ] ) ] >
8:  single piece groupoid with rays: < Group( [ IdentityTransformation ] ), 
[ IdentityTransformation, Transformation( [ 1, 2, 4, 3 ] ) ], 
[ IdentityTransformation, Transformation( [ 1, 2, 4, 3 ] ) ] >
gap> IsGroupoidWithMonoidObjects( rag );
true
gap> orag := ObjectList( rag );;
gap> hs := Homset( rag, orag[3], orag[4] );;  
gap> Display( hs );                  
<homset Transformation( [ 1, 1, 1, 3 ] ) -> Transformation( [ 1, 1, 1 ] )
  with elements:
[Transformation( [ 1, 2, 4, 3 ] ) : Transformation( [ 1, 1, 1, 3 ] ) -> 
Transformation( [ 1, 1, 1 ] )]

gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );;  
gap> STOP_TEST( "gpd.tst", 10000 );

