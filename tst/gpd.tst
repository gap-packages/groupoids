##############################################################################
##
#W  gpd.tst                       Gpd Package                    Chris Wensley
##
##  version 1.36, 23/11/2015   
##
#Y  Copyright (C) 2000-2015, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

## SubSection 4.1.1 
gap> gpd_infolevel_saved := InfoLevel( InfoGpd );; 
gap> SetInfoLevel( InfoGpd, 0 );; 
gap> s4 := Group( (1,2,3,4), (3,4) );; 
gap> d8 := Subgroup( s4, [ (1,2,3,4), (1,3) ] );;
gap> SetName( s4, "s4" );  SetName( d8, "d8" ); 
gap> Gs4 := SinglePieceGroupoid( s4, [-15 .. -11] ); 
single piece groupoid: < s4, [ -15 .. -11 ] >
gap> Gd8 := Groupoid( d8, [-9,-8,-7] );
single piece groupoid: < d8, [ -9, -8, -7 ] >
gap> c6 := Group( (5,6,7)(8,9) );;
gap> SetName( c6, "c6" );
gap> Gc6 := DomainWithSingleObject( c6, -6 );
single piece groupoid: < c6, [ -6 ] >
gap> SetName( Gs4, "Gs4" );  SetName( Gd8, "Gd8" );  SetName( Gc6, "Gc6" );  

## SubSection 4.1.2
gap> ObjectList( Gs4 );    
[ -15 .. -11 ]
gap> RootObject( Gd8 );
-9
gap> RootGroup( Gc6 );
c6
gap> ObjectGroup( Gs4, -11 );
s4

## SubSection 4.1.3
gap> f2 := FreeGroup( 2 );;
gap> Gf2 := Groupoid( f2, -22 );;
gap> SetName( f2, "f2" );  SetName( Gf2, "Gf2" ); 
gap> q8 := SmallGroup( 8, 4 );;
gap> Gq8 := Groupoid( q8, [ -28, -27 ] );;
gap> SetName( q8, "q8" );  SetName( Gq8, "Gq8" );
gap> sl43 := SpecialLinearGroup( 4, 3 );;
gap> Gsl43 := SinglePieceGroupoid( sl43, [ -35..-31 ] );;
gap> SetName( sl43, "sl43" );  SetName( Gsl43, "Gsl43" );
gap> [ IsMatrixGroupoid( Gsl43 ), IsFpGroupoid( Gf2 ), 
>      IsPcGroupoid( Gq8 ), IsPermGroupoid( Gs4 ) ]; 
[ true, true, true, true ]

## SubSection 4.1.4
gap> U3 := UnionOfPieces( [ Gc6, Gd8, Gs4 ] );;
gap> Display( U3 );
groupoid with 3 pieces:
< objects: [ -15 .. -11 ]
    group: s4 = <[ (1,2,3,4), (3,4) ]> >
< objects: [ -9, -8, -7 ]
    group: d8 = <[ (1,2,3,4), (1,3) ]> >
< objects: [ -6 ]
    group: c6 = <[ (5,6,7)(8,9) ]> >
gap> Pieces( U3 );
[ Gs4, Gd8, Gc6 ]
gap> ObjectList( U3 );
[ -15, -14, -13, -12, -11, -9, -8, -7, -6 ]
gap> U2 := Groupoid( [ Gf2, Gq8 ] );;
gap> [ Size(Gs4), Size(Gd8), Size(Gc6), Size(U3) ];
[ 600, 72, 6, 678 ]
gap> [ Size(Gf2), Size(Gq8), Size(U2) ];           
[ infinity, 32, infinity ]
gap> U5 := UnionOfPieces( [ U3, U2 ] );
groupoid with 5 pieces:
[ Gq8, Gf2, Gs4, Gd8, Gc6 ]
gap> Display( U5 );
groupoid with 5 pieces:
< objects: [ -28, -27 ]
    group: q8 = <[ f1, f2, f3 ]> >
< objects: [ -22 ]
    group: f2 = <[ f1, f2 ]> >
< objects: [ -15 .. -11 ]
    group: s4 = <[ (1,2,3,4), (3,4) ]> >
< objects: [ -9, -8, -7 ]
    group: d8 = <[ (1,2,3,4), (1,3) ]> >
< objects: [ -6 ]
    group: c6 = <[ (5,6,7)(8,9) ]> >
gap> V5 := ReplaceOnePieceInUnion( U5, 3, Gsl43 ); 
groupoid with 5 pieces:
[ Gsl43, Gq8, Gf2, Gd8, Gc6 ]
gap> ObjectList(V5);             
[ -35, -34, -33, -32, -31, -28, -27, -22, -9, -8, -7, -6 ]
gap> U5 = V5;
false
gap> W5 := ReplaceOnePieceInUnion( V5, Gc6, Gs4 ); 
groupoid with 5 pieces:
[ Gsl43, Gq8, Gf2, Gs4, Gd8 ]

## SubSection 4.1.5
gap> Hd8 := HomogeneousGroupoid( Gd8, [ [-12,-11,-10], [-16,-15,-14] ] );
homogeneous groupoid with 2 pieces:
1:  single piece groupoid: < d8, [ -16, -15, -14 ] >
2:  single piece groupoid: < d8, [ -12, -11, -10 ] >
gap> IsHomogeneousDomainWithObjects(Hd8);               
true
gap> Hc6 := HomogeneousDiscreteGroupoid( c6, [-7..-4] ); 
homogeneous, discrete groupoid: < c6, [ -7 .. -4 ] >
gap> RepresentationsOfObject(Gd8);
[ "IsComponentObjectRep", "IsAttributeStoringRep", "IsMWOSinglePieceRep" ]
gap> RepresentationsOfObject(Hd8);
[ "IsComponentObjectRep", "IsAttributeStoringRep", "IsPiecesRep" ]
gap> RepresentationsOfObject(Hc6);
[ "IsComponentObjectRep", "IsAttributeStoringRep", 
  "IsHomogeneousDiscreteGroupoidRep" ]
gap> ktpo := KnownTruePropertiesOfObject(Hc6);; 
gap> ans := 
> [ "CanEasilyCompareElements", "CanEasilySortElements", 
>   "IsDuplicateFree", "IsAssociative", "IsCommutative", 
>   "IsDiscreteDomainWithObjects", "IsHomogeneousDomainWithObjects" ];;
gap> ForAll( ans, a -> ( a in ktpo ) ); 
true

## Section 4.2 : Groupoid elements: stars; costars; homsets ###

## SubSection 4.2.1
gap> e1 := Arrow( Gd8, (1,2,3,4), -9, -8 );
[(1,2,3,4) : -9 -> -8]
gap> e2 := Arrow( Gd8, (1,3), -8, -7 );
[(1,3) : -8 -> -7]
gap> Print( [ ElementOfArrow(e2), TailOfArrow(e2), HeadOfArrow(e2) ], "\n" );
[ (1,3), -8, -7 ]
gap> prod := e1*e2;
[(1,2)(3,4) : -9 -> -7]
gap> e2*e1;
fail
gap> e3 := Arrow( Gd8, (2,4), -7, -9 );;
gap> loop := prod*e3;
[(1,4,3,2) : -9 -> -9]
gap> loop^2;
[(1,3)(2,4) : -9 -> -9]

## SubSection 4.2.2
gap> i8 := IdentityArrow( Gd8, -8 );
[() : -8 -> -8]
gap> [ e1*i8, i8*e1, e1^-1]; 
[ [(1,2,3,4) : -9 -> -8], fail, [(1,4,3,2) : -8 -> -9] ]
gap> Order(i8);
1
gap> Order(loop);
4

## SubSection 4.2.3
gap> star9 := ObjectStar( Gd8, -9 );
<star at [ -9 ] with group d8>
gap> Size( star9 ); 
24
gap> for e in star9 do
>      if ( Order( ElementOfArrow(e) ) = 4 ) then Print( e, "\n" ); fi;
>    od;
[(1,4,3,2) : -9 -> -9]
[(1,4,3,2) : -9 -> -8]
[(1,4,3,2) : -9 -> -7]
[(1,2,3,4) : -9 -> -9]
[(1,2,3,4) : -9 -> -8]
[(1,2,3,4) : -9 -> -7]
gap> costar6 := ObjectCostar( Gc6, -6 );
<costar at [ -6 ] with group c6>
gap> Size( costar6 ); 
6
gap> hsetq8 := Homset( Gq8, -28, -27 );
<homset -28 -> -27 with group q8>
gap> for e in hsetq8 do Print(e,"\n"); od;
[<identity> of ... : -28 -> -27]
[f3 : -28 -> -27]
[f2 : -28 -> -27]
[f2*f3 : -28 -> -27]
[f1 : -28 -> -27]
[f1*f3 : -28 -> -27]
[f1*f2 : -28 -> -27]
[f1*f2*f3 : -28 -> -27]

### Section 4.3 : Subgroupoids ###

## SubSection 4.3.1
gap> c4 := Subgroup( d8, [ (1,2,3,4) ] );;
gap> k4 := Subgroup( d8, [ (1,2)(3,4), (1,3)(2,4) ] );;
gap> SetName( c4, "c4" );  SetName( k4, "k4" );
gap> Ud8 := Subgroupoid( Gd8, [ [ k4, [-9] ], [ c4, [-8,-7] ] ] );;
gap> SetName( Ud8, "Ud8" );
gap> Display( Ud8 );
groupoid with 2 pieces:
< objects: [ -9 ]
    group: k4 = <[ (1,2)(3,4), (1,3)(2,4) ]> >
< objects: [ -8, -7 ]
    group: c4 = <[ (1,2,3,4) ]> >
gap> [ Parent( Ud8 ), IsWide( Gd8, Ud8 ) ]; 
[ Gd8, true ]
gap> genf2b := List( GeneratorsOfGroup(f2), g -> g^2 );
[ f1^2, f2^2 ]
gap> f2b := Subgroup( f2, genf2b );;
gap> SubgroupoidByPieces( U2, [ [q8,[-27]], [f2b,[-22]] ] );
groupoid with 2 pieces:
1:  single piece groupoid: < q8, [ -27 ] >
2:  single piece groupoid: < Group( [ f1^2, f2^2 ] ), [ -22 ] >
gap> IsSubgroupoid( Gf2, Groupoid( f2b, [-22] ) );
true
gap> FullSubgroupoid( U3, [-7,-6] );
groupoid with 2 pieces:
1:  single piece groupoid: < d8, [ -7 ] >
2:  single piece groupoid: < c6, [ -6 ] >
gap> DiscreteSubgroupoid( U3, [ c4, k4 ], [-9,-7] );
groupoid with 2 pieces:
1:  single piece groupoid: < c4, [ -9 ] >
2:  single piece groupoid: < k4, [ -7 ] >
gap> FullTrivialSubgroupoid( Ud8 );
groupoid with 2 pieces:
1:  single piece groupoid: < id(k4), [ -9 ] >
2:  single piece groupoid: < id(c4), [ -8, -7 ] >
gap> MaximalDiscreteSubgroupoid(U2);
groupoid with 3 pieces:
1:  single piece groupoid: < q8, [ -28 ] >
2:  single piece groupoid: < q8, [ -27 ] >
3:  single piece groupoid: < f2, [ -22 ] >

## SubSection 4.3.2
gap> Hs4 := FullSubgroupoid( Gs4, [-14,-13,-12] );; 
gap> SetName( Hs4, "Hs4" ); 
gap> Hd8a := SubgroupoidWithRays( Hs4, d8, [(),(2,3),(3,4)] );
single piece groupoid with rays: < d8, [ -14, -13, -12 ], [ (), (2,3), (3,4) 
 ] >
gap> hs1413 := Homset( Hd8a, -14, -13 );
<homset -14 -> -13 with group d8>
gap> for e in hs1413 do  Print(e,", "); od;  Print( "\n");
[(2,3) : -14 -> -13], [(2,4,3) : -14 -> -13], [(1,2,4,3) : -14 -> -13], [
(1,2,3) : -14 -> -13], [(1,4,2) : -14 -> -13], [(1,4) : -14 -> -13], [
(1,3,4) : -14 -> -13], [(1,3,4,2) : -14 -> -13], 
gap> Hd8b := SubgroupoidWithRays( Hs4, d8, [(),(1,2,3),(1,2,4)] );
single piece groupoid with rays: < d8, [ -14, -13, -12 ], 
[ (), (1,2,3), (1,2,4) ] >
gap> Hd8a = Hd8b; 
true
gap> RayElementsOfGroupoid( Hd8b ); 
[ (), (1,2,3), (1,2,4) ]
gap> Parent( Hd8a );
Hs4
gap> Ancestor( Hd8a ); 
Gs4
gap> Fd8a := FullSubgroupoid( Hd8a, [-13,-12] );            
single piece groupoid with rays: < Group( [ (1,3,2,4), (1,2) ] ), 
[ -13, -12 ], [ (), (2,4,3) ] >
gap> Kd8a := SubgroupoidWithRays( Fd8a, k4, [ (), (1,3) ] ); 
single piece groupoid with rays: < k4, [ -13, -12 ], [ (), (1,3) ] >

### Section 4.4 : Left, Right and Double Cosets ###

## SubSection 4.4.1
gap> re2 := RightCoset( Gd8, Ud8, e2 );
RightCoset(single piece groupoid: < c4, [ -8, -7 ] >,[(1,3) : -8 -> -7])
gap> for x in re2 do Print( x, "\n" ); od;
[(1,3) : -8 -> -7]
[(1,3) : -7 -> -7]
[(2,4) : -8 -> -7]
[(2,4) : -7 -> -7]
[(1,4)(2,3) : -8 -> -7]
[(1,4)(2,3) : -7 -> -7]
[(1,2)(3,4) : -8 -> -7]
[(1,2)(3,4) : -7 -> -7]
gap> rcrd8 := RightCosetRepresentatives( Gd8, Ud8 );
[ [() : -9 -> -9], [() : -9 -> -8], [() : -9 -> -7], [(2,4) : -9 -> -9], 
  [(2,4) : -9 -> -8], [(2,4) : -9 -> -7], [() : -8 -> -9], [() : -8 -> -8], 
  [() : -8 -> -7], [(2,4) : -8 -> -9], [(2,4) : -8 -> -8], [(2,4) : -8 -> -7] 
 ]
gap> lcr7 := LeftCosetRepresentativesFromObject( Gd8, Ud8, -7 );
[ [() : -7 -> -9], [(2,4) : -7 -> -9], [() : -7 -> -8], [(2,4) : -7 -> -8] ]

### Section 4.5 : conjugation ###

## SubSection 4.5.1
gap> x := Arrow( Gd8, (1,3), -9, -9 );; 
gap> y := Arrow( Gd8, (1,2,3,4), -8, -9 );; 
gap> z := Arrow( Gd8, (1,2)(3,4), -9, -7 );; 
gap> w := Arrow( Gd8, (1,2,3,4), -7, -8 );; 
gap> ##  conjugation with elements x, y, and z in Gd8: 
gap> ConjugateArrow(x,y);
[(2,4) : -8 -> -8]
gap> ConjugateArrow(x,z);
[(2,4) : -7 -> -7]
gap> ConjugateArrow(y,x);
[() : -8 -> -9]
gap> ConjugateArrow(y,z);
[(2,4) : -8 -> -7]
gap> ConjugateArrow(z,x);
[(1,4,3,2) : -9 -> -7]
gap> ConjugateArrow(z,y);
[(2,4) : -8 -> -7]
gap> ConjugateArrow(w,y);
[(1,3)(2,4) : -7 -> -9]
gap> ConjugateArrow(w,z);
[(1,3) : -9 -> -8]

## SubSection 4.5.2
gap> u := Arrow( Gs4, (1,2,3), -15, -13 ); 
[(1,2,3) : -15 -> -13]
gap> gensa := GeneratorsOfGroupoid( Hd8a );
[ [(1,2,3,4) : -14 -> -14], [(1,3) : -14 -> -14], [(2,3) : -14 -> -13], 
  [(3,4) : -14 -> -12] ]
gap> imsa := List( gensa, g -> ConjugateArrow( g, u ) ); 
[ [(1,2,3,4) : -14 -> -14], [(1,3) : -14 -> -14], [(1,3) : -14 -> -15], 
  [(3,4) : -14 -> -12] ]
gap> C := SinglePieceGroupoidByGenerators( Gs4, imsa ); 
single piece groupoid with rays: < Group( [ (1,4,3,2), (1,3) ] ), 
[ -15, -14, -12 ], [ (), (1,3), (1,4,3) ] >

## SubSection 4.5.3
gap> ConjugateGroupoid( Hd8a, u^-1 ); 
single piece groupoid with rays: < Group( [ (1,4,3,2), (1,3) ] ), 
[ -15, -14, -12 ], [ (), (1,3), (1,4,3) ] >
gap> SetInfoLevel( InfoGpd, gpd_infolevel_saved );;  

#############################################################################
##
#E  gpd.tst . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
