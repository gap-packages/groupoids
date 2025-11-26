############################################################################
##
#W  double.tst               Groupoids Package                 Chris Wensley
##

gap> START_TEST( "groupoids package: tecnotes.tst" );
gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

## make tecnotes.tst independent of gpd.tst and gpdhom.tst
gap> a4 := Group( (1,2,3), (2,3,4) );; 
gap> d8 := Group( (5,6,7,8), (5,7) );;
gap> SetName( a4, "a4" );  SetName( d8, "d8" ); 
gap> Ga4 := SinglePieceGroupoid( a4, [-15 .. -11] );; 
gap> Gd8 := Groupoid( d8, [-9,-8,-7] );;
gap> e1 := GroupoidElement( Gd8, (5,6,7,8), -9, -8 );;
gap> c6 := Group( (11,12,13)(14,15) );;
gap> SetName( c6, "c6" );
gap> Gc6 := MagmaWithSingleObject( c6, -10 );;
gap> SetName( Ga4, "Ga4" );  SetName( Gd8, "Gd8" );  SetName( Gc6, "Gc6" );  
gap> U3 := UnionOfPieces( [ Ga4, Gc6, Gd8 ] );;

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
gap> U2 := Groupoid( [ Gf2, Gq8 ] );;
gap> U5 := UnionOfPieces( [ U3, U2 ] );;

gap> Ha4 := SubgroupoidByObjects( Ga4, [-14,-13,-12] );;  
gap> SetName( Ha4, "Ha4" );
gap> k4 := Subgroup( a4, [ (1,2)(3,4), (1,3)(2,4) ] );;
gap> SetName( k4, "k4" );
gap> Kk4 := SubgroupoidWithRays( Ha4, k4, [ (), (1,3,4), (1,4)(2,3) ] );;
gap> SetName( Kk4, "Kk4" );
gap> gend8 := GeneratorsOfGroup( d8 );;
gap> imh := [ (1,4)(2,3), () ];;
gap> h := GroupHomomorphismByImages( d8, a4, gend8, imh );;                     
gap> hom9 := GroupoidHomomorphism( Gd8, Kk4, h, [-14,-13,-12],
>                 [ (), (1,3,4), (1,4)(2,3) ] );;
gap> perm1 := [-13,-12,-14];;
gap> aut1 := GroupoidAutomorphismByObjectPerm( Ha4, perm1 );; 


## Section 9.1 
gap> CategoriesOfObject( Gd8 );
[ "IsListOrCollection", "IsCollection", "IsExtLElement", 
  "CategoryCollections(IsExtLElement)", "IsExtRElement", 
  "CategoryCollections(IsExtRElement)", 
  "CategoryCollections(IsMultiplicativeElement)", "IsGeneralizedDomain", 
  "IsDomainWithObjects", 
  "CategoryCollections(IsMultiplicativeElementWithObjects)", 
  "CategoryCollections(IsMultiplicativeElementWithObjectsAndOnes)", 
  "CategoryCollections(IsMultiplicativeElementWithObjectsAndInverses)", 
  "CategoryCollections(IsGroupoidElement)", "IsMagmaWithObjects", 
  "IsSemigroupWithObjects", "IsMonoidWithObjects", "IsGroupoid" ]
gap> FamilyObj( Gd8 );                     
<Family: "CollectionsFamily(...)">
gap> Display(last);   
name:
    CollectionsFamily(...)
required filters:
    IsCollection
implied filters:
    IsListOrCollection
    IsCollection
    IsExtLElement
    CategoryCollections(IsExtLElement)
    IsExtRElement
    CategoryCollections(IsExtRElement)
    CategoryCollections(IsMultiplicativeElement)
    IsOddAdditiveNestingDepthObject
    CategoryCollections(IsMultiplicativeElementWithObjects)
    CategoryCollections(IsMultiplicativeElementWithObjectsAndOnes)
    CategoryCollections(IsMultiplicativeElementWithObjectsAndInverses)
    CategoryCollections(IsGroupoidElement)
gap> KnownAttributesOfObject( Gd8 ); 
[ "Name", "ObjectList", "GeneratorsOfMagmaWithObjects", 
  "GeneratorsOfGroupoid" ]
gap> KnownTruePropertiesOfObject( Gd8 ); 
[ "IsDuplicateFree", "IsAssociative", "IsSinglePieceDomain", 
  "IsDirectProductWithCompleteDigraphDomain" ]
gap> RepresentationsOfObject( Gd8 );
[ "IsComponentObjectRep", "IsAttributeStoringRep", "IsMWOSinglePieceRep" ]
gap> RepresentationsOfObject( U5 ); 
[ "IsComponentObjectRep", "IsAttributeStoringRep", "IsPiecesRep" ]

gap> e1;
[(5,6,7,8) : -9 -> -8]
gap> CategoriesOfObject( e1 );
[ "IsExtLElement", "IsExtRElement", "IsMultiplicativeElement", 
  "IsMultiplicativeElementWithObjects", 
  "IsMultiplicativeElementWithObjectsAndOnes", 
  "IsMultiplicativeElementWithObjectsAndInverses", "IsGroupoidElement" ]
gap> FamilyObj( e1 );
<Family: "IsGroupoidElementFamily">

## Section 9.2
gap> hom9;
groupoid homomorphism : Gd8 -> Kk4
[ [ [(5,6,7,8) : -9 -> -9], [(5,7) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,4)(2,3) : -14 -> -14], [() : -14 -> -14], [(1,3,4) : -14 -> -13], 
      [(1,4)(2,3) : -14 -> -12] ] ]
gap> CategoriesOfObject( hom9 );
[ "IsExtLElement", "IsExtRElement", "IsMultiplicativeElement", 
  "IsMultiplicativeElementWithOne", "IsMultiplicativeElementWithInverse", 
  "IsAssociativeElement", "IsGeneralMapping", "IsSPGeneralMapping", 
  "IsGeneralMappingWithObjects", "IsSPGeneralMappingWithObjects", 
  "IsGroupoidHomomorphism" ]
gap> FamilyObj( hom9 );
<Family: "GroupoidHomomorphismFamily">
gap> Display( last );
name:
    GroupoidHomomorphismFamily
required filters:
    IsGroupoidHomomorphism
implied filters:
    CanEasilyCompareElements
    HasCanEasilyCompareElements
    CanEasilySortElements
    HasCanEasilySortElements
    IsExtLElement
    IsExtRElement
    IsMultiplicativeElement
    IsMultiplicativeElementWithOne
    IsMultiplicativeElementWithInverse
    IsAssociativeElement
    IsGeneralMapping
    IsSPGeneralMapping
    RespectsMultiplication
    HasRespectsMultiplication
    IsGeneralMappingWithObjects
    IsSPGeneralMappingWithObjects
    IsGroupoidHomomorphism
gap> FamilyObj( hom9 ) = FamilyObj( aut1 );
true
gap> KnownAttributesOfObject( hom9 );
[ "Range", "Source", "MappingGeneratorsImages", "MappingToSinglePieceData", 
  "ImagesOfObjects", "ImageElementsOfRays", "RootGroupHomomorphism" ]
gap> KnownTruePropertiesOfObject( hom9 );
[ "CanEasilyCompareElements", "CanEasilySortElements", 
  "RespectsMultiplication", "IsGroupWithObjectsHomomorphism", 
  "IsGeneralMappingToSinglePiece", "IsGeneralMappingFromSinglePiece", 
  "IsInjectiveOnObjects", "IsSurjectiveOnObjects" ]
gap> KnownTruePropertiesOfObject( aut1 );
[ "CanEasilyCompareElements", "CanEasilySortElements", "IsEndoGeneralMapping",
  "RespectsMultiplication", "IsGroupWithObjectsHomomorphism", 
  "IsGeneralMappingToSinglePiece", "IsGeneralMappingFromSinglePiece", 
  "IsInjectiveOnObjects", "IsSurjectiveOnObjects", 
  "IsEndomorphismWithObjects", "IsAutomorphismWithObjects", 
  "IsGroupoidAutomorphismByObjectPerm" ]
gap> KnownAttributesOfObject( hom9 );
[ "Range", "Source", "MappingGeneratorsImages", "MappingToSinglePieceData", 
  "ImagesOfObjects", "ImageElementsOfRays", "RootGroupHomomorphism" ]
gap> KnownAttributesOfObject( aut1 );
[ "Order", "Range", "Source", "MappingGeneratorsImages", "AutomorphismDomain",
  "MappingToSinglePieceData", "ImagesOfObjects", "ImageElementsOfRays", 
  "RootGroupHomomorphism" ]




gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
gap> STOP_TEST( "tecnotes.tst", 10000 );
