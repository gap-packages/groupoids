############################################################################
##
#W  gpd.tst                 groupoids Package                  Chris Wensley
##

gap> START_TEST( "groupoids package: gpd.tst" );
gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

## SubSection 4.1.1 
gap> a4 := Group( (1,2,3), (2,3,4) );; 
gap> d8 := Group( (5,6,7,8), (5,7) );;
gap> SetName( a4, "a4" );  SetName( d8, "d8" ); 
gap> Ga4 := SinglePieceGroupoid( a4, [-15 .. -11] ); 
single piece groupoid: < a4, [ -15 .. -11 ] >
gap> Gd8 := Groupoid( d8, [-9,-8,-7] );
single piece groupoid: < d8, [ -9, -8, -7 ] >
gap> c6 := Group( (11,12,13)(14,15) );;
gap> SetName( c6, "c6" );
gap> Gc6 := MagmaWithSingleObject( c6, -10 );
single piece groupoid: < c6, [ -10 ] >
gap> IsGroupoid( Gc6 ); 
true
gap> SetName( Ga4, "Ga4" );  SetName( Gd8, "Gd8" );  SetName( Gc6, "Gc6" );  

## SubSection 4.1.2 
gap> ObjectList( Ga4 );
[ -15 .. -11 ] 
gap> f2 := FreeGroup(2);;
gap> Gf2d8 := Groupoid( d8, GeneratorsOfGroup(f2) );
single piece groupoid: < d8, [ f1, f2 ] >
gap> Arrow( Gf2d8, (6,8), f2.1, f2.2 );
[(6,8) : f1 -> f2]
gap> Gabc := Groupoid( c6, [ "a", "b", "c" ] );
single piece groupoid: < c6, [ "a", "b", "c" ] >
gap> Arrow( Gabc, (14,15), "c", "b" );
[(14,15) : c -> b]

## SubSection 4.1.3
gap> f2 := FreeGroup( 2 );;
gap> Gf2 := Groupoid( f2, -20 );;
gap> SetName( f2, "f2" );  SetName( Gf2, "Gf2" ); 
gap> q8 := QuaternionGroup( 8 );;
gap> genq8 := GeneratorsOfGroup( q8 );;
gap> x := genq8[1];;  y := genq8[2];;
gap> Gq8 := Groupoid( q8, [ -19, -18, -17 ] );;
gap> SetName( q8, "q8" );  SetName( Gq8, "Gq8" );
gap> sl43 := SpecialLinearGroup( 4, 3 );;
gap> Gsl43 := SinglePieceGroupoid( sl43, [-23,-22,-21] );;
gap> SetName( sl43, "sl43" );  SetName( Gsl43, "Gsl43" );
gap> [ IsMatrixGroupoid( Gsl43 ), IsFpGroupoid( Gf2 ), IsFreeGroupoid( Gf2 ),
>      IsPcGroupoid( Gq8 ), IsPermGroupoid( Ga4 ) ]; 
[ true, true, true, true, true ]

## SubSection 4.1.4
gap> U3 := UnionOfPieces( [ Ga4, Gc6, Gd8 ] );;
gap> Display( U3 );
groupoid with 3 pieces:
< objects: [ -15 .. -11 ]
    group: a4 = <[ (1,2,3), (2,3,4) ]> >
< objects: [ -10 ]
    group: c6 = <[ (11,12,13)(14,15) ]> >
< objects: [ -9, -8, -7 ]
    group: d8 = <[ (5,6,7,8), (5,7) ]> >
gap> Pieces( U3 );
[ Ga4, Gc6, Gd8 ]
gap> ObjectList( U3 );
[ -15, -14, -13, -12, -11, -10, -9, -8, -7 ]
gap> [ Size(Ga4), Size(Gd8), Size(Gc6), Size(U3) ];
[ 300, 72, 6, 378 ]
gap> U2 := Groupoid( [ Gf2, Gq8 ] );;
gap> [ Size(Gf2), Size(Gq8), Size(U2) ];           
[ infinity, 72, infinity ]
gap> U5 := UnionOfPieces( [ U3, U2 ] );
groupoid with 5 pieces:
[ Gf2, Gq8, Ga4, Gc6, Gd8 ]
gap> V3 := ReplaceOnePieceInUnion( U3, Gd8, Gq8 ); 
groupoid with 3 pieces:
[ Gq8, Ga4, Gc6 ]
gap> ObjectList( V3 );             
[ -19, -18, -17, -15, -14, -13, -12, -11, -10 ]

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
    [ [ [(5,6,7,8) : -39 -> -39], [(5,7) : -39 -> -39], [() : -39 -> -38], 
          [() : -39 -> -37] ], 
      [ [(5,6,7,8) : -36 -> -36], [(5,7) : -36 -> -36], [() : -36 -> -35], 
          [() : -36 -> -34] ] ], groupoid homomorphism : 
    [ [ [(5,6,7,8) : -39 -> -39], [(5,7) : -39 -> -39], [() : -39 -> -38], 
          [() : -39 -> -37] ], 
      [ [(5,6,7,8) : -33 -> -33], [(5,7) : -33 -> -33], [() : -33 -> -32], 
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
gap> Embedding( prod, 2 );                        
groupoid homomorphism : 
[ [ [(11,12,13)(14,15) : -10 -> -10] ], 
  [ [(5,6,7)(8,9) : [ -9, -10 ] -> [ -9, -10 ]] ] ]
gap> ## note that the first embedding has not yet been created
gap> DirectProductInfo( prod );
rec( embeddings := [ , groupoid homomorphism : 
        [ [ [(11,12,13)(14,15) : -10 -> -10] ], 
          [ [(5,6,7)(8,9) : [ -9, -10 ] -> [ -9, -10 ]] ] ] ], first := Gd8, 
  groupoids := [ Gd8, Gc6 ], groups := [ d8, c6 ], 
  objectlists := [ [ -9, -8, -7 ], [ -10 ] ], projections := [  ] )
gap> Projection( prod, 1 );  
groupoid homomorphism : 
[ [ [(1,2,3,4) : [ -9, -10 ] -> [ -9, -10 ]], 
      [(1,3) : [ -9, -10 ] -> [ -9, -10 ]], 
      [(5,6,7)(8,9) : [ -9, -10 ] -> [ -9, -10 ]], 
      [() : [ -9, -10 ] -> [ -8, -10 ]], [() : [ -9, -10 ] -> [ -7, -10 ]] ], 
  [ [(5,6,7,8) : -9 -> -9], [(5,7) : -9 -> -9], [() : -9 -> -9], 
      [() : -9 -> -8], [() : -9 -> -7] ] ]

## Section 4.2 : Groupoid elements: stars; costars; homsets ###

## SubSection 4.2.1
gap> e1 := GroupoidElement( Gd8, (5,6,7,8), -9, -8 );
[(5,6,7,8) : -9 -> -8]
gap> e2 := Arrow( Gd8, (5,7), -8, -7 );
[(5,7) : -8 -> -7]
gap> Print( [ ElementOfArrow(e1), TailOfArrow(e1), HeadOfArrow(e1) ], "\n" );
[ (5,6,7,8), -9, -8 ]
gap> IsGroupoidElement( e1 );
true
gap> e1e2 := e1*e2;
[(5,6)(7,8) : -9 -> -7]
gap> e2*e1;
fail
gap> e3 := Arrow( Gd8, (6,8), -7, -9 );;
gap> loop := e1e2*e3;
[(5,8,7,6) : -9 -> -9]
gap> loop^2;
[(5,7)(6,8) : -9 -> -9]

## SubSection 4.2.2
gap> i8 := IdentityArrow( Gd8, -8 );
[() : -8 -> -8]
gap> [ e1*i8, i8*e1, e1^-1]; 
[ [(5,6,7,8) : -9 -> -8], fail, [(5,8,7,6) : -8 -> -9] ]

## SubSection 4.2.3
gap> [ i8, loop ]; 
[ [() : -8 -> -8], [(5,8,7,6) : -9 -> -9] ]
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
19 : [(5,6,7,8) : -9 -> -9]
20 : [(5,6,7,8) : -9 -> -8]
21 : [(5,6,7,8) : -9 -> -7]
22 : [(5,6)(7,8) : -9 -> -9]
23 : [(5,6)(7,8) : -9 -> -8]
24 : [(5,6)(7,8) : -9 -> -7]
gap> costar12 := ObjectCostar( Ga4, -12 );
<costar at -12 with vertex group a4>
gap> Size( costar12 );
60
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

## SubSection 4.3.2 : SubgroupoidByObjects/Subgroup
gap> Ha4 := SubgroupoidByObjects( Ga4, [-14,-13,-12] );  
single piece groupoid: < a4, [ -14, -13, -12 ] >
gap> SetName( Ha4, "Ha4" );
gap> IsSubgroupoid( Ga4, Ha4 );
true
gap> c3a := Subgroup( a4, [ (1,2,3) ] );; 
gap> SetName( c3a, "c3a" );
gap> Hc3a := SubgroupoidBySubgroup( Ha4, c3a );
single piece groupoid: < c3a, [ -14, -13, -12 ] >
gap> [ IsWideSubgroupoid( Ga4, Ha4 ), IsWideSubgroupoid( Ha4, Hc3a ) ];
[ false, true ]
gap> [ IsFullSubgroupoid( Ga4, Ha4 ), IsFullSubgroupoid( Ha4, Hc3a ) ];
[ true, false ]

## SubSection 4.3.3 : SubgroupoidWithRays
gap> k4 := Subgroup( a4, [ (1,2)(3,4), (1,3)(2,4) ] );;
gap> SetName( k4, "k4" );
gap> Gk4 := SubgroupoidWithRays( Ga4, k4, 
>               [ (), (1,2,3), (1,2,4), (1,3,4), (2,3,4) ] );
single piece groupoid with rays: < k4, [ -15 .. -11 ], 
[ (), (1,2,3), (1,2,4), (1,3,4), (2,3,4) ] >
gap> SetName( Gk4, "Gk4" );
gap> RaysOfGroupoid( Gk4 );       
[ (), (1,2,3), (1,2,4), (1,3,4), (2,3,4) ]
gap> RayArrowsOfGroupoid( Gk4 );  
[ [() : -15 -> -15], [(1,2,3) : -15 -> -14], [(1,2,4) : -15 -> -13],
  [(1,3,4) : -15 -> -12], [(2,3,4) : -15 -> -11] ]
gap> IsDirectProductWithCompleteDigraph( Gk4 );
false
gap> ObjectGroup( Gk4, -14 );
Group([ (1,4)(2,3), (1,2)(3,4) ])
gap> c2 := Subgroup( k4, [ (1,4)(2,3) ] );;  SetName( c2, "c2" );
gap> Gc2 := Subgroupoid( Gk4, c2, [ (), (1,3,4), (1,4,3), (1,2,3), (1,3,2) ] );
single piece groupoid with rays: < c2, [ -15 .. -11 ], 
[ (), (1,3,4), (1,4,3), (1,2,3), (1,3,2) ] >

## SubSection 4.3.4 : SubgroupoidByPieces 
gap> Display( Ga4 );
perm single piece groupoid: Ga4
  objects: [ -15 .. -11 ]
    group: a4 = <[ (1,2,3), (2,3,4) ]>
gap> c3b := Subgroup( a4, [ (1,2,4) ] );;
gap> SetName( c3b, "c3b" );
gap> pieces := [ [ c3a, [-14] ], [ c3b, [-13,-12], [(),(1,4)(2,3)] ] ];;
gap> Jc3 := Subgroupoid( Ha4, pieces );;
gap> SetName( Jc3, "Jc3" );
gap> Display( Jc3 );
groupoid with 2 pieces:
< objects: [ -14 ]
    group: c3a = <[ (1,2,3) ]> >
<     objects: [ -13, -12 ]
   parent gpd: single piece groupoid: < a4, [ -13, -12 ] >
   root group: c3b = <[ (1,2,4) ]>
         rays: [ (), (1,4)(2,3) ]
gap> [ Parent( Jc3 ), IsWideSubgroupoid( Ha4, Jc3 ) ]; 
[ Ga4, true ]
gap> pJc3 := Pieces( Jc3 );;
gap> SetName( pJc3[1], "Jc3a" );  SetName( pJc3[2], "Jc3b" );
gap> U2;
groupoid with 2 pieces:
[ Gf2, Gq8 ]
gap> genf2b := List( GeneratorsOfGroup(f2), g -> g^2 );
[ f1^2, f2^2 ]
gap> f2b := Subgroup( f2, genf2b );;
gap> JU2 := SubgroupoidByPieces( U2, [ [f2b,[-20]], [q8,[-17]] ] );
groupoid with 2 pieces:
1:  single piece groupoid: < Group( [ f1^2, f2^2 ] ), [ -20 ] >
2:  single piece groupoid: < q8, [ -17 ] >
gap> [ IsWideSubgroupoid(U2,JU2), IsSubgroupoid(Gf2,Groupoid(f2b,[-20])) ];
[ false, true ]
gap> pJU2 := Pieces( JU2 );; 
gap> SetName( pJU2[1], "JU2a" );  SetName( pJU2[2], "JU2b" );

## SubSection 4.3.5
gap> T1 := UnionOfPieces( [Ha4,U2] );;  Pieces( T1 );
[ Gf2, Gq8, Ha4 ]
gap> T2 := UnionOfPieces( [Jc3,JU2] );;  Pieces( T2 );
[ JU2a, JU2b, Jc3a, Jc3b ]
gap> PiecePositions( T1, T2 );                                               
[ 1, 2, 3, 3 ]
gap> InclusionMappingGroupoids( T1, T2 );
groupoid homomorphism from several pieces : 
groupoid homomorphism : JU2a -> Gf2
[ [ [ [f1^2 : -20 -> -20], [f2^2 : -20 -> -20] ], 
      [ [f1^2 : -20 -> -20], [f2^2 : -20 -> -20] ] ] ]
groupoid homomorphism : JU2b -> Gq8
[ [ [ [x : -17 -> -17], [y : -17 -> -17], [y2 : -17 -> -17] ], 
      [ [x : -17 -> -17], [y : -17 -> -17], [y2 : -17 -> -17] ] ] ]
groupoid homomorphism : 
[ [ [ [(1,2,3) : -14 -> -14] ], [ [(1,2,3) : -14 -> -14] ] ], 
  [ [ [(1,2,4) : -13 -> -13], [(1,4)(2,3) : -13 -> -12] ], 
      [ [(1,2,4) : -13 -> -13], [(1,4)(2,3) : -13 -> -12] ] ] ]

## SubSection 4.3.6
gap> FullTrivialSubgroupoid( Jc3 );
groupoid with 2 pieces:
1:  single piece groupoid: < id(c3a), [ -14 ] >
2:  single piece groupoid: < id(c3b), [ -13, -12 ] >
gap> DiscreteTrivialSubgroupoid( Gd8 );
homogeneous, discrete groupoid: < id(d8), [ -9, -8, -7 ] >

## SubSection 4.3.7
gap> U3;
groupoid with 3 pieces:
[ Ga4, Gc6, Gd8 ]
gap> c4 := Subgroup( d8, [ (5,6,7,8) ] );;  SetName( c4, "c4" );
gap> DiscreteSubgroupoid( U3, [ c3a, c3b, c6, c4 ], [-15,-13,-10,-7] );
groupoid with 4 pieces:
1:  single piece groupoid: < c3a, [ -15 ] >
2:  single piece groupoid: < c3b, [ -13 ] >
3:  single piece groupoid: < c6, [ -10 ] >
4:  single piece groupoid: < c4, [ -7 ] >
gap> HomogeneousDiscreteSubgroupoid( Ga4, k4, [-15,-13,-11] ); 
homogeneous, discrete groupoid: < a4, [ -15, -13, -11 ] >
gap> MaximalDiscreteSubgroupoid( Jc3 );
groupoid with 3 pieces:
1:  single piece groupoid: < c3a, [ -14 ] >
2:  single piece groupoid: < c3b, [ -13 ] >
3:  single piece groupoid: < Group( [ (1,4,3) ] ), [ -12 ] >

## SubSection 4.3.8
gap> a1 := Arrow( Gk4, (1,2)(3,4), -15, -15 );;
gap> a2 := Arrow( Gk4, (1,3,2), -15, -13 );;
gap> a3 := Arrow( Gk4, (2,3,4), -15, -11 );;
gap> SinglePieceSubgroupoidByGenerators( Gk4, [a1,a2,a3] );
single piece groupoid with rays: < Group( [ (1,2)(3,4) ] ), [ -15, -13, -11  ],
[ (), (1,3,2), (2,3,4) ] >

### Section 4.4 : Left, Right and Double Cosets ###

## SubSection 4.4.1
gap> e4 := Arrow( Jc3, (2,4,3), -13, -12 );;;                            
gap> re4 := RightCoset( Ha4, Jc3, e4 );
<right coset of Jc3b with representative [(2,4,3) : -13 -> -12]>
gap> Perform( re4, Display );
[(2,4,3) : -13 -> -12]
[(1,3,4) : -12 -> -12]
[(1,3,2) : -13 -> -12]
[(1,4,3) : -12 -> -12]
[(1,4)(2,3) : -13 -> -12]
[() : -12 -> -12]
gap> rcra4 := RightCosetRepresentatives( Ha4, Jc3 );
[ [() : -14 -> -14], [(1,2)(3,4) : -14 -> -14], [(1,3)(2,4) : -14 -> -14], 
  [(1,4)(2,3) : -14 -> -14], [() : -14 -> -13], [(1,2)(3,4) : -14 -> -13], 
  [(1,3)(2,4) : -14 -> -13], [(1,4)(2,3) : -14 -> -13], [() : -14 -> -12], 
  [(1,2)(3,4) : -14 -> -12], [(1,3)(2,4) : -14 -> -12], 
  [(1,4)(2,3) : -14 -> -12], [() : -13 -> -13], [(1,2)(3,4) : -13 -> -13], 
  [(1,3)(2,4) : -13 -> -13], [(1,4)(2,3) : -13 -> -13], [() : -12 -> -12], 
  [(1,2)(3,4) : -12 -> -12], [(1,3)(2,4) : -12 -> -12], 
  [(1,4)(2,3) : -12 -> -12], [() : -13 -> -14], [(1,2)(3,4) : -13 -> -14], 
  [(1,3)(2,4) : -13 -> -14], [(1,4)(2,3) : -13 -> -14] ]
gap> le4 := LeftCoset( Ha4, Jc3, e4 ); 
<left coset of Jc3b with representative [(1,4,2) : -13 -> -13]>
gap> Perform( le4, Display );
[(1,4,2) : -13 -> -13]
[(2,4,3) : -13 -> -12]
[() : -13 -> -13]
[(1,4)(2,3) : -13 -> -12]
[(1,2,4) : -13 -> -13]
[(1,3,2) : -13 -> -12]
gap> lcra4 := LeftCosetRepresentatives( Ha4, Jc3 );
[ [() : -14 -> -14], [(1,2)(3,4) : -14 -> -14], [(1,3)(2,4) : -14 -> -14], 
  [(1,4)(2,3) : -14 -> -14], [() : -13 -> -14], [(1,2)(3,4) : -13 -> -14], 
  [(1,3)(2,4) : -13 -> -14], [(1,4)(2,3) : -13 -> -14], [() : -12 -> -14], 
  [(1,2)(3,4) : -12 -> -14], [(1,3)(2,4) : -12 -> -14], 
  [(1,4)(2,3) : -12 -> -14], [() : -13 -> -13], [(1,2)(3,4) : -13 -> -13], 
  [(1,3)(2,4) : -13 -> -13], [(1,4)(2,3) : -13 -> -13], [() : -12 -> -12], 
  [(1,2)(3,4) : -12 -> -12], [(1,3)(2,4) : -12 -> -12], 
  [(1,4)(2,3) : -12 -> -12], [() : -14 -> -13], [(1,2)(3,4) : -14 -> -13], 
  [(1,3)(2,4) : -14 -> -13], [(1,4)(2,3) : -14 -> -13] ]
gap> lcr11 := LeftCosetRepresentativesFromObject( Ha4, Jc3, -12 );
[ [() : -12 -> -14], [(1,2)(3,4) : -12 -> -14], [(1,3)(2,4) : -12 -> -14], 
  [(1,4)(2,3) : -12 -> -14], [() : -12 -> -12], [(1,2)(3,4) : -12 -> -12], 
  [(1,3)(2,4) : -12 -> -12], [(1,4)(2,3) : -12 -> -12] ]
gap> de4 := DoubleCoset( Ha4, Jc3, Jc3, e4 );
<double coset of [ Jc3b, Jc3b ] with representative [(1,4,2) : -13 -> -13]>
gap> Perform( de4, Display );
[() : -13 -> -13]
[(1,4)(2,3) : -13 -> -12]
[(1,4)(2,3) : -12 -> -13]
[() : -12 -> -12]
[(1,4,2) : -13 -> -13]
[(2,4,3) : -13 -> -12]
[(1,2,3) : -12 -> -13]
[(1,3,4) : -12 -> -12]
[(1,2,4) : -13 -> -13]
[(1,3,2) : -13 -> -12]
[(2,3,4) : -12 -> -13]
[(1,4,3) : -12 -> -12]
gap> dcra4 := DoubleCosetRepresentatives( Ha4, Jc3, Jc3 );
[ [() : -14 -> -14], [(1,2)(3,4) : -14 -> -14], [() : -14 -> -13], 
  [(1,2)(3,4) : -14 -> -13], [() : -13 -> -14], [(1,2)(3,4) : -13 -> -14], 
  [() : -13 -> -13], [(1,2)(3,4) : -13 -> -13] ]

### Section 4.5 : conjugation ###

## SubSection 4.5.1
gap> p := Arrow( Gd8, (5,7), -9, -9 );; 
gap> q := Arrow( Gd8, (5,6,7,8), -8, -9 );; 
gap> r := Arrow( Gd8, (5,6)(7,8), -9, -7 );; 
gap> s := Arrow( Gd8, (5,6,7,8), -7, -8 );; 
gap> ##  conjugation with elements p, q, and r in Gd8: 
gap> p^q;
[(6,8) : -8 -> -8]
gap> p^r;
[(6,8) : -7 -> -7]
gap> q^p;
[() : -8 -> -9]
gap> q^r;
[(6,8) : -8 -> -7]
gap> r^p;
[(5,8,7,6) : -9 -> -7]
gap> r^q;
[(6,8) : -8 -> -7]
gap> s^p;                           
[(5,6,7,8) : -7 -> -8]
gap> s^q;
[(5,7)(6,8) : -7 -> -9]
gap> s^r;
[(5,7) : -9 -> -8]

## SubSection 4.5.2
gap> a5 := Arrow( Ga4, (1,2,3), -13, -12 ); 
[(1,2,3) : -13 -> -12]
gap> ConjugateGroupoid( Gk4, a5 );
single piece groupoid with rays: < Group( [ (1,2)(3,4), (1,3)(2,4) ] ), 
[ -15, -14, -13, -12, -11 ], [ (), (1,2,3), (1,2)(3,4), (1,3)(2,4), (2,3,4) ] >

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
gap> d8 := Group( (5,6,7,8), (5,7) );; 
gap> ed8 := Elements( d8 );; 
gap> Rd8 := SinglePieceGroupoidWithRays( d8, ed8, ed8 );
single piece groupoid with rays: < Group( [ (5,6,7,8), (5,7) ] ), 
[ (), (6,8), (5,6)(7,8), (5,6,7,8), (5,7), (5,7)(6,8), (5,8,7,6), (5,8)(6,7) 
 ], [ (), (6,8), (5,6)(7,8), (5,6,7,8), (5,7), (5,7)(6,8), (5,8,7,6), 
  (5,8)(6,7) ] >
gap> Homset( Rd8, (6,8), (5,7) );
<homset (6,8) -> (5,7) with head group Group( [ (5,8,7,6), (5,7) ] )>
gap> Display( last ); 
<homset (6,8) -> (5,7) with elements:
[(5,7)(6,8) : (6,8) -> (5,7)]
[(5,7) : (6,8) -> (5,7)]
[() : (6,8) -> (5,7)]
[(6,8) : (6,8) -> (5,7)]
[(5,8,7,6) : (6,8) -> (5,7)]
[(5,8)(6,7) : (6,8) -> (5,7)]
[(5,6,7,8) : (6,8) -> (5,7)]
[(5,6)(7,8) : (6,8) -> (5,7)]

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

