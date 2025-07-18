############################################################################
##
#W  mwohom.gi              GAP4 package `groupoids'            Chris Wensley
#W                                                              & Emma Moore
##  
##  This file contains generic methods for mappings of magmas with objects

#############################################################################
##  Standard error messages

MAGMA_HOMOMORPHISM_CONSTRUCTORS := Concatenation(
    "The standard operations which construct a magma mapping are:\n",
    "1.  HomomorphismFromSinglePiece( src, rng, hom, imobs );\n", 
    "2.  HomomorphismToSinglePiece( src, rng, list of [hom,imobs]'s );\n",
    "3.  HomomorphismByUnion( src, rng, list of disjoint mappings );\n", 
    "4.  MagmaWithObjectsHomomorphism( one of previous parameter options );" );

#############################################################################
##
#F  MagmaWithObjectsHomomorphism(<g1>,<g2>,<maps>)          union of mappings 
#F  MagmaWithObjectsHomomorphism(<g1>,<g2>,<piece images>) single piece range 
#F  MagmaWithObjectsHomomorphism(<g1>,<g2>,<hom>,<imobs>)    connected source 
##
InstallGlobalFunction( MagmaWithObjectsHomomorphism, function( arg )

    local nargs, id, rays; 

    nargs := Length( arg );
    if ( ( nargs < 3 ) or not IsMagmaWithObjects( arg[1] ) or 
         ( nargs > 4 ) or not IsMagmaWithObjects( arg[2] ) ) then 
        Info( InfoGroupoids, 1, MAGMA_HOMOMORPHISM_CONSTRUCTORS );
        return fail;
    fi;
    # mwo, mwo, magma mapping, object images
    if ( ( nargs = 4 ) and IsSinglePiece( arg[1] ) 
                       and IsMagmaHomomorphism( arg[3] ) 
                       and IsHomogeneousList( arg[4] ) ) then 
        Info( InfoGroupoids, 2, "HomomorphismFromSinglePiece" );
        return HomomorphismFromSinglePiece(arg[1],arg[2],arg[3],arg[4]); 
    # mwo, mwo, list of mappings
    elif ( ( nargs = 3 ) and IsHomogeneousList( arg[3] ) 
            and IsMagmaWithObjectsHomomorphism( arg[3][1] ) ) then
        Info( InfoGroupoids, 2, "HomomorphismByUnion" );
        return HomomorphismByUnion( arg[1], arg[2], arg[3] );
    # mwo, mwo, list of [mapping,[images of objects]] pairs
    elif ( ( nargs = 3 ) and IsHomogeneousList( arg[3] ) 
           and IsList( arg[3][1] ) and IsList( arg[3][1][1] ) ) then 
        Info( InfoGroupoids, 2, "HomomorphismToSinglePiece" );
        return HomomorphismToSinglePiece( arg[1], arg[2], arg[3] );
    else
        Info( InfoGroupoids, 1, MAGMA_HOMOMORPHISM_CONSTRUCTORS );
        return fail;
    fi;
end );

#############################################################################
##
#M  MappingWithObjectsByFunction 
##
InstallMethod( MappingWithObjectsByFunction,
    "generic method for a mapping by function of connected magmas", true,
    [ IsMagmaWithObjects, IsMagmaWithObjects, IsFunction, IsHomogeneousList ], 
    0,
function( m1, m2, fun, imo )

    local map, ok;

    map := rec(); 
    ObjectifyWithAttributes( map, IsMWOMappingToSinglePieceType, 
        Source, m1, 
        Range, m2, 
        UnderlyingFunction, fun, 
        ImagesOfObjects, imo,   
        RespectsMultiplication, true, 
        IsGeneralMappingToSinglePiece, true, 
        IsMappingWithObjectsByFunction, true );
    return map; 
end );

#############################################################################
##
#M  HomomorphismFromSinglePieceNC 
#M  HomomorphismFromSinglePiece 
##
InstallMethod( HomomorphismFromSinglePieceNC,
    "generic method for a mapping of connected magmas", true,
    [ IsSinglePiece, IsSinglePiece, IsMagmaHomomorphism, IsHomogeneousList ], 
    0,
function( m1, m2, hom, imo )
    return HomomorphismToSinglePieceNC( m1, m2, [ [ hom, imo ] ] );
end );

InstallMethod( HomomorphismFromSinglePiece,
    "generic method for a mapping of connected magmas", true,
    [ IsSinglePiece, IsSinglePiece, IsMagmaHomomorphism, IsHomogeneousList ], 
    0,
function( m1, m2, hom, imo )

    local o1, o2, no2, mag1, mag2;

    Info( InfoGroupoids, 3, "homomorphism from single piece magma" ); 
    o1 := m1!.objects;
    o2 := m2!.objects;
    no2 := Length( o2 );
    if not ( Length( imo ) = Length( o1 ) ) then
        Error( "object images of incorrect length" );
    fi;
    if not ForAll( imo, o -> o in o2 ) then
        Error( "object images not in the objects of m2" );
    fi;
    mag1 := m1!.magma; 
    mag2 := m2!.magma;
    if not ( ( Source(hom) = mag1 ) and ( Range(hom) = mag2 ) ) then
        Error( "hom not a mapping m1!.magma -> m2!.magma" );
    fi;
    return HomomorphismFromSinglePieceNC( m1, m2, hom, imo );
end );

#############################################################################
##
#M  HomomorphismToSinglePieceNC  
#M  HomomorphismToSinglePiece
##
InstallMethod( HomomorphismToSinglePieceNC,
    "generic method for a mapping to a single piece magma", true,
    [ IsMagmaWithObjects, IsSinglePiece, IsHomogeneousList ], 0,
function( mag1, mag2, images )

    local map, ok, imo;

    Info( InfoGroupoids, 3, "homomorphism to a single piece magma - NC" ); 
    map := rec(); 
    ObjectifyWithAttributes( map, IsMWOMappingToSinglePieceType, 
        Source, mag1, 
        Range, mag2, 
        MappingToSinglePieceData, images,
        ImagesOfObjects, Flat( List( images, L -> L[2] ) ),   
        RespectsMultiplication, true, 
        IsHomomorphismToSinglePiece, true );
    ok := IsInjectiveOnObjects( map ); 
    ok := IsSurjectiveOnObjects( map ); 
    if ( ( "IsMonoidWithObjects" in CategoriesOfObject( mag1 ) ) and 
         ( "IsMonoidWithObjects" in CategoriesOfObject( mag2 ) ) ) then 
        SetIsMonoidWithObjectsHomomorphism( map, true ); 
    elif ( ( "IsSemigroupWithObjects" in CategoriesOfObject( mag1 ) ) and 
           ( "IsSemigroupWithObjects" in CategoriesOfObject( mag2 ) ) ) then 
        SetIsSemigroupWithObjectsHomomorphism( map, true );
    elif ( ( "IsMagmaWithObjects" in CategoriesOfObject( mag1 ) ) and 
           ( "IsMagmaWithObjects" in CategoriesOfObject( mag2 ) ) ) then 
        SetIsMagmaWithObjectsHomomorphism( map, true );
    fi; 
    ok := IsHomomorphismFromSinglePiece( map ); 
    return map; 
end );

InstallMethod( HomomorphismToSinglePiece, 
    "generic method for a mapping to a single piece magma", true,
    [ IsMagmaWithObjects, IsSinglePiece, IsHomogeneousList ], 0,
function( mwo1, mwo2, maps )

    local pieces, o2, m2, j, pj, h, mor;

    Info( InfoGroupoids, 3, "homomorphism to a single piece magma:", maps ); 
    pieces := Pieces( mwo1 ); 
    o2 := mwo2!.objects;
    m2 := mwo2!.magma;
    for j in [1..Length(pieces)] do 
        pj := pieces[j];
        h := maps[j][1];
        if not IsMagmaHomomorphism( h ) then
            Error( "h is not a magma homomorphism" );
        fi;
        if not ( ( Source(h) = pj!.magma ) and ( Range(h) = m2 ) ) then
            Error( "homs not a mapping m1!.magma -> m2!.magma" );
        fi;
        if not ( Length( maps[j][2] ) = Length( pj!.objects ) ) then 
            Error( "incorrect length for maps of objects" ); 
        fi;
        if not IsSubset( o2, maps[j][2] ) then 
            Error( "objects in the image not objects in m2" );
        fi;
    od;
    return HomomorphismToSinglePieceNC( mwo1, mwo2, maps ); 
end );

InstallMethod( HomomorphismToSinglePieceNC,
    "generic method for a mapping to a single piece magma", true,
    [ IsGroupoid, IsSinglePiece, IsHomogeneousList ], 0,
function( gpd1, gpd2, homs )

    local data, map, ok, imo;

    Info( InfoGroupoids, 3, "homomorphism to a single piece groupoid - NC" ); 
    data := List( homs, h -> MappingToSinglePieceData(h)[1] ); 
    map := rec(); 
    ObjectifyWithAttributes( map, IsGroupoidMappingToSinglePieceType, 
        Source, gpd1, 
        Range, gpd2, 
        MappingToSinglePieceData, data,  
        MappingToSinglePieceMaps, homs,  
        RespectsMultiplication, true, 
        IsHomomorphismToSinglePiece, true );
    ok := IsInjectiveOnObjects( map ); 
    ok := IsSurjectiveOnObjects( map ); 
    ok := IsGroupWithObjectsHomomorphism( map ); 
    ok := IsHomomorphismFromSinglePiece( map );
    return map; 
end );

InstallMethod( HomomorphismToSinglePiece,
    "generic method for a mapping to a single piece magma", true,
    [ IsGroupoid, IsSinglePiece, IsHomogeneousList ], 0,
function( mwo1, mwo2, homs )

    local pieces, o2, j, h;

    Info( InfoGroupoids, 3, "morphism to a single piece groupoid:\n", homs ); 
    pieces := Pieces( mwo1 ); 
    if not ( Length( pieces ) = Length( homs ) ) then 
        Error( "there should be one homomorphism for each piece in mwo1" ); 
    fi;
    o2 := mwo2!.objects;
    for j in [1..Length(pieces)] do 
        h := homs[j];
        if not IsMagmaWithObjectsHomomorphism( h ) then
            Error( "h is not a magma with objects homomorphism" );
        fi;
        if not ( ( Source(h) = pieces[j] ) and ( Range(h) = mwo2 ) ) then
            Error( "sources/ranges of homs do not agree with mwo1/mwo2" );
        fi;
    od;
    return HomomorphismToSinglePieceNC( mwo1, mwo2, homs );
end );

#############################################################################
##
#M  HomomorphismByUnionNC  
#M  HomomorphismByUnion
##
InstallMethod( HomomorphismByUnionNC, 
    "generic method for a magma mapping", true,
    [ IsMagmaWithObjects, IsMagmaWithObjects, IsList ], 0,  
function( mag1, mag2, images )

    local type, map, inj, pieces1, nc1, pieces2, nc2, obs1, 
          part, i, m, src;

    if IsSinglePiece( mag2 ) then 
        Info( InfoGroupoids, 1, "better to use single piece function" );
        return HomomorphismToSinglePiece( mag1, mag2, images );
    fi; 
    if ( ( "IsGroupoid" in CategoriesOfObject( mag1 ) ) and 
         ( "IsGroupoid" in CategoriesOfObject( mag2 ) ) ) then 
        type := IsGroupoidMappingWithPiecesType; 
    else 
        type := IsMWOMappingWithPiecesType; 
    fi;
    map := rec(); 
    ObjectifyWithAttributes( map, type, 
        Source, mag1,
        Range, mag2,
        PiecesOfMapping, images,
        IsGeneralMappingToSinglePiece, false );
    inj := IsInjectiveOnObjects( map ); 
    pieces1 := Pieces( mag1 );
    nc1 := Length( pieces1 );
    pieces2 := Pieces( mag2 );
    nc2 := Length( pieces2 );
    obs1 := List( pieces1, g -> g!.objects[1] ); 
    part := ListWithIdenticalEntries( nc2, 0 );
    for i in [1..nc2] do
        m := images[i];
        src := Source( m );
        if IsSinglePiece( src ) then
            part[i] := [ Position( obs1, src!.objects[1] ) ];
        else
            part[i] := List( Pieces( src ),  
                       c -> Position( obs1, c!.objects[1] ) );
        fi;
    od;
    Info( InfoGroupoids, 3, "part = ", part );
    SetPartitionOfSource( map, part );
    return map; 
end );

InstallMethod( HomomorphismByUnion, 
    "generic method for a magma mapping", true,
    [ IsMagmaWithObjects, IsMagmaWithObjects, IsList ], 0, 
function( mag1, mag2, maps )

    local pieces1, nc1, pieces2, nc2, lenmaps, lenpieces, npieces, expand, 
          src1, img1, g, ppos, pos2, piecesmap, L, i, j, k, m, filt, 
          mapj, srcj;

    if not ForAll( maps, IsGeneralMappingWithObjects ) then 
        Error( "all maps should have IsGeneralMappingWithObjects" ); 
    fi; 
    if IsSinglePiece( mag2 ) then 
        Info( InfoGroupoids, 1, "better to use single piece function" );
        return HomomorphismToSinglePieceNC( mag1, mag2, maps ); 
    fi;
    pieces1 := Pieces( mag1 );
    nc1 := Length( pieces1 );
    pieces2 := Pieces( mag2 );
    nc2 := Length( pieces2 );
    lenmaps := Length( maps ); 
    lenpieces := ListWithIdenticalEntries( lenmaps, 0 );
    for i in [1..lenmaps] do 
        m := maps[i];
        if not HasPiecesOfMapping( m ) then 
            lenpieces[i] := 1; 
        else 
            lenpieces[i] := Length( PiecesOfMapping( m ) );
        fi;
    od;
    npieces := Sum( lenpieces );    
    expand := ListWithIdenticalEntries( npieces, 0 );
    j := 0;
    for i in [1..Length(maps)] do
        m := maps[i];
        if HasPiecesOfMapping( m ) then
            piecesmap := PiecesOfMapping( m );
            for k in [1..Length( piecesmap )] do
                j := j+1;
                expand[j] := piecesmap[k];
            od;
        else
            j := j+1;
            expand[j] := m;
        fi;
    od;
    Info( InfoGroupoids, 3, "expanded maps:", expand );
    src1 := List( expand, Source );
    img1 := UnionOfPieces( List( maps, m -> ImagesSource( m ) ) );
    ppos := PiecePositions( mag2, img1 );
    Info( InfoGroupoids, 3, " ppos = ", ppos );
    if ( fail in ppos ) then
        Error( "not all m have source in mag1" );
    fi;
    ## construct the constituent mappings
    piecesmap := ListWithIdenticalEntries( nc2, 0 );
    for j in [1..nc2] do
        filt := Filtered( [1..npieces], k -> ppos[k] = j );
        mapj := maps{filt};
        srcj := UnionOfPieces( src1{filt} );
        piecesmap[j] := HomomorphismToSinglePiece( srcj, pieces2[j], mapj );
    od;

##    ##  more efficient to use PieceNrOfObject here ??
##    pos2 := List( maps, m -> Position( pieces2, 
##                      PieceOfObject( mag2, Range(m)!.objects[1] ) ) );
##    if ( fail in pos2 ) then
##        Error( "not all m have range in mag2" );
##    fi;
##    if IsDuplicateFree( pos2 ) then
##        ## reorder if necessary
##        Info( InfoGroupoids, 2, "duplicate free case" );
##        L := [1..nc2];
##        SortParallel( pos2, L );
##        piecesmap := List( L, j -> expand[j] );
##        return HomomorphismByUnionNC( mag1, mag2, piecesmap );
##    else
##        ## construct the constituent mappings
##        piecesmap := ListWithIdenticalEntries( nc2, 0 );
##        for j in [1..nc2] do
##            filt := Filtered( pos2, i -> (i=j) );
##            mapj := List( filt, i -> maps[i] );
##            if ( Length( filt ) = 1 ) then
##                piecesmap[j] := mapj[1]; 
##            else
##                if ( Length( filt ) = nc1 ) then
##                    src := mag1;
##                else
##                    src := UnionOfPieces( List(filt), i -> Source(maps[i]) );
##                fi;
##                piecesmap[j] := 
##                    HomomorphismToSinglePiece( src, pieces2[j], mapj );
##            fi;
##        od;
        return HomomorphismByUnionNC( mag1, mag2, piecesmap );
##    fi;
end );

#############################################################################
##
#M  IsomorphismNewObjects
##
InstallMethod( IsomorphismNewObjects, "for a single piece mwo", true,
    [ IsSinglePiece, IsHomogeneousList ], 0,
function( m1, ob2 )

    local isgpd, iso, ob1, mag, m2, id;

    ob1 := m1!.objects; 
    if not ( Length(ob1) = Length(ob2) ) then
        Error( "object sets have different lengths" );
    fi;
    mag := m1!.magma;
    m2 := SinglePieceMagmaWithObjects( mag, ShallowCopy( Set( ob2 ) ) );
    id := IdentityMapping( mag );
    iso := HomomorphismFromSinglePiece( m1, m2, id, ob2 ); 
    return iso;
end );

InstallMethod( IsomorphismNewObjects, "generic method for a magma", true,
    [ IsMagmaWithObjects, IsHomogeneousList ], 0,
function( m1, ob2 )

    local isos, pieces1, nc1, i, m2, iso;

    pieces1 := Pieces( m1 );
    nc1 := Length( pieces1 );
    if not ( Length(ob2) = nc1 ) then
        Error( "new list of lists of objects has incorrect length" );
    fi;
    isos := ListWithIdenticalEntries( nc1, 0 );
    for i in [1..nc1] do 
        isos[i] := IsomorphismNewObjects( pieces1[i], ob2[i] );
    od;
    m2 := UnionOfPieces( List( isos, Range ) );
    iso := HomomorphismByUnion( m1, m2, isos );
    return iso;
end );

#############################################################################
##
#M  IdentityMapping
##
InstallOtherMethod( IdentityMapping, "for a magma with objects", true, 
    [ IsMagmaWithObjects ], 0,
function( mwo )

    local pieces, gp, gens, id, len, homs, iso;

    if IsSinglePiece( mwo ) then
        iso := HomomorphismToSinglePieceNC
            ( mwo, mwo, [ [mwo!.objects, IdentityMapping(mwo!.magma)] ] ); 
    elif ( HasIsHomogeneousDiscreteGroupoid( mwo ) 
           and IsHomogeneousDiscreteGroupoid( mwo ) ) then 
        id := IdentityMapping( Pieces(mwo)[1]!.magma ); 
        len := Length( ObjectList( mwo ) ); 
        homs := ListWithIdenticalEntries( len, id ); 
        iso := GroupoidAutomorphismByGroupAutos( mwo, homs ); 
    else
        pieces := List( Pieces( mwo ), IdentityMapping );
        iso := MagmaWithObjectsHomomorphism( mwo, mwo, pieces );
    fi;
    SetIsInjectiveOnObjects( iso, true );
    SetIsSurjectiveOnObjects( iso, true );
    return iso;
end );

#############################################################################
##
#M  ImagesOfObjects
##
InstallMethod( ImagesOfObjects, "for a magma mapping", true,
    [ IsMagmaWithObjectsHomomorphism and IsHomomorphismToSinglePiece ], 0,
function( map )

    local data, src, obs, ims, c, i, obc, imc, j, pos;

    data := MappingToSinglePieceData( map );
    if ( Length( data ) = 1 ) then
        return data[1][2];
    else
        src := Source( map );
        obs := ObjectList( src );
        ims := ShallowCopy( obs );
        c := Pieces( src ); 
        for i in [1..Length(c)] do 
            obc := c[i]!.objects;
            imc := data[i][2];
            for j in [1..Length(obc)] do
                pos := Position( obs, obc[j] );
                ims[pos] := imc[j];
            od;
        od;
        return ims;
    fi;
end );

InstallMethod( ImagesOfObjects, "for a magma mapping", true,
    [ IsMagmaWithObjectsHomomorphism ], 0,
function( map )

    local src, obs, ims, i, c, obc, o, pos;

    src := Source( map );
    obs := ObjectList( src );
    ims := ShallowCopy( obs );
    for c in PiecesOfMapping( map ) do
        obc := Source( c )!.objects;
        for i in [1..Length(obc)] do
            pos := Position( obs, obc[i] );
            ims[pos] := ImagesOfObjects( c )[i];
        od;
    od;
    return ims;
end ); 

#############################################################################
##
#M  IsSemigroupWithObjectsHomomorphism
#M  IsMonoidWithObjectsHomomorphism 
#M  IsGroupWithObjectsHomomorphism
##
InstallMethod( IsSemigroupWithObjectsHomomorphism, 
    "for a mapping with objects", true, [ IsMagmaWithObjectsHomomorphism ], 0,
function( map ) 
    return 
        ( ( "IsSemigroupWithObjects" in CategoriesOfObject( Source(map) ) ) and  
          ( "IsSemigroupWithObjects" in CategoriesOfObject( Range(map) ) ) ); 
end );

InstallMethod( IsMonoidWithObjectsHomomorphism, "for a mapping with objects", 
    true, [ IsMagmaWithObjectsHomomorphism ], 0,
function( map ) 
    return 
        ( ( "IsMonoidWithObjects" in CategoriesOfObject( Source(map) ) ) and 
          ( "IsMonoidWithObjects" in CategoriesOfObject( Range(map) ) ) ); 
end );

InstallMethod( IsGroupWithObjectsHomomorphism, "for a mapping with objects", 
    true, [ IsMagmaWithObjectsHomomorphism ], 0,
function( map ) 
    return ( IsGroupoid( Source(map) ) and IsGroupoid( Range(map) ) ); 
end );

#############################################################################
##
#M  IsGeneralMappingToSinglePiece
#M  IsGeneralMappingFromSinglePiece 
##
InstallMethod( IsGeneralMappingToSinglePiece, 
    "for a mapping with objects", true, [ IsMagmaWithObjectsHomomorphism ], 0,
function( map ) 
    return IsSinglePiece( Range( map ) ); 
end );

InstallMethod( IsGeneralMappingFromSinglePiece, 
    "for a mapping with objects", true, [ IsMagmaWithObjectsHomomorphism ], 0,
function( map ) 
    return IsSinglePiece( Source( map ) ); 
end );

#############################################################################
##
#M  IsConstantOnObjects
#M  IsInjectiveOnObjects 
#M  IsSurjectiveOnObjects 
#M  IsBijectiveOnObjects
##
InstallMethod( IsConstantOnObjects,
    "generic method for a magma mapping with objects", true,
    [ IsMagmaWithObjectsHomomorphism ], 0,
function( map )
    return Source( map )!.objects = ImagesOfObjects( map ); 
end );

InstallMethod( IsInjectiveOnObjects, "for a mapping with objects", true,
    [ IsHomomorphismToSinglePiece ], 0,
function( map )

    local images, imo;

    imo := ImagesOfObjects( map );
    if not ( imo = fail ) then 
        return IsDuplicateFree( Flat( imo ) );
    elif IsHomomorphismToSinglePiece( map ) then
        images := MappingToSinglePieceData( map );
        imo := Flat( List( images, L -> L[2] ) ); 
        return IsDuplicateFree( imo ); 
    elif ( HasIsGeneralMappingFromHomogeneousDiscrete( map ) 
           and IsGeneralMappingFromHomogeneousDiscrete( map ) ) then 
        return IsDuplicateFree( map!.oims ); 
    else  ## mapping has pieces
        imo := List( PiecesOfMapping( map ), 
                     m -> MappingToSinglePieceData(m)[2] );
        return IsDuplicateFree( Flat( imo ) );
    fi;
end );

InstallMethod( IsInjectiveOnObjects, "for a mapping with objects", true,
    [ IsMagmaWithObjectsHomomorphism ], 0,
function( map )
    return ForAll( PiecesOfMapping( map ), IsInjectiveOnObjects ); 
end );

InstallMethod( IsInjectiveOnObjects, 
    "for a mapping from a homogeneous, discrete groupoid", true,
    [ IsGroupoidHomomorphismFromHomogeneousDiscreteRep ], 0,
function( map ) 
    return IsDuplicateFree( ImagesOfObjects( map ) ); 
end );

InstallMethod( IsSurjectiveOnObjects, "for a mapping with objects", true,
    [ IsHomomorphismToSinglePiece ], 0,
function( map ) 

    local obr, images, imo;

    obr := ObjectList( Range( map ) ); 
    if HasImagesOfObjects( map ) then 
        imo := Flat( ImagesOfObjects( map ) );
    elif IsHomomorphismToSinglePiece( map ) then
        images := MappingToSinglePieceData( map );
        imo := Flat( List( images, L -> L[2] ) );
    else  ## mapping has pieces
        imo := Flat( List( PiecesOfMapping( map ), 
                     m -> MappingToSinglePieceData(m)[1][2] ) );
    fi;
    return ( Set(imo) = obr );
end );

InstallMethod( IsSurjectiveOnObjects, "for a mapping with objects", true,
    [ IsMagmaWithObjectsHomomorphism ], 0,
function( map )
    return ForAll( PiecesOfMapping( map ), IsSurjectiveOnObjects ); 
end );

InstallMethod( IsSurjectiveOnObjects, 
    "for a mapping from a homogeneous, discrete groupoid", true,
    [ IsGroupoidHomomorphismFromHomogeneousDiscreteRep ], 0,
function( map ) 

    local obr, images, imo;

    obr := ObjectList( Range( map ) ); 
    imo := ImagesOfObjects( map );
    return ( Set(imo) = obr ); 
end );

InstallMethod( IsBijectiveOnObjects, "for a mapping with objects", true, 
    [ IsMagmaWithObjectsHomomorphism ], 0,
function( map ) 
    return IsInjectiveOnObjects( map ) and IsSurjectiveOnObjects( map ); 
end ); 

##############################################################################
##
#M  IsEndomorphismWithObjects( map ) . . . . . . . . . .  for a magma mapping
#M  IsAutomorphismWithObjects( map ) . . . . . . . . . .  for a magma mapping
##
InstallMethod( IsEndomorphismWithObjects, "for a magma mapping", true, 
    [ IsMagmaWithObjectsHomomorphism ], 0,
function( map )
    return ( Source( map ) = Range( map ) );
end );

InstallMethod( IsAutomorphismWithObjects, "for a magma mapping", true, 
    [ IsMagmaWithObjectsHomomorphism ], 0,
function( map )
    return ( IsEndomorphismWithObjects( map ) 
             and IsInjectiveOnObjects( map ) 
             and IsSurjectiveOnObjects( map ) );
end );

#############################################################################
##
#M  InverseGeneralMapping
#M  InverseMapping
##
InstallMethod( InverseMapping, "for a magma mapping", true,
    [ IsMagmaWithObjectsHomomorphism and IsInjectiveOnObjects 
                                and IsSurjectiveOnObjects ], 0,
function( map )
    return InverseGeneralMapping( map );
end );

InstallMethod( InverseGeneralMapping, "for a magma mapping", true,
    [ IsMagmaWithObjectsHomomorphism ], 0,
function( map )

    local pieces, isos, inv;

    Info( InfoGroupoids, 3, "InverseGeneralMapping for magma mappings" );
    pieces := PiecesOfMapping( map );
    isos := List( pieces, InverseGeneralMapping );
    inv := HomomorphismByUnion( Range(map), Source(map), isos );
    SetIsInjectiveOnObjects( inv, true );
    SetIsSurjectiveOnObjects( inv, true );
    return inv;
end );

InstallMethod( InverseGeneralMapping, "for a single piece mapping", true,
    [ IsMagmaWithObjectsHomomorphism and IsHomomorphismToSinglePiece ], 0,
function( map )

    local m1, m2, ob1, ob2, nob, hom21, len, sc1, sc2, obhom1, inv;

    if not (IsInjectiveOnObjects(map) and IsSurjectiveOnObjects(map)) then
        Error( "mapping with objects not bijective" );
    fi;
    Info( InfoGroupoids, 3, "InverseGeneralMapping for single piece mappings" );
    m1 := Source( map );
    if not IsSinglePiece( m1 ) then
        Error( "source is not single piece" );
    fi;
    m2 := Range( map );
    ob1 := m1!.objects;
    ob2 := m2!.objects;
    nob := Length( ob1 );
    obhom1 := MappingToSinglePieceData( map );
    len := Length( obhom1 );
    sc1 := ShallowCopy( ob1 );
    sc2 := ShallowCopy( obhom1[1][2] );
    SortParallel( sc2, sc1 );
    hom21 := InverseGeneralMapping( obhom1[1][1] );
    SetIsTotal( hom21, true );
    SetRespectsMultiplication( hom21, true );
    SetIsInjective( hom21, true );
    SetIsSurjective( hom21, true );
    inv := HomomorphismFromSinglePiece( m2, m1, hom21, sc1 );
    SetIsInjectiveOnObjects( inv, true );
    SetIsSurjectiveOnObjects( inv, true );
    return inv;
end );

InstallMethod( InverseGeneralMapping, "for a groupoid isomorphism", true,
    [ IsGroupWithObjectsHomomorphism and IsHomomorphismFromSinglePiece], 0,
function( iso )

    local src, rng, obss, obsr, nobs, imobs, L, invobs, pos1, pi, 
          rhom, rgps, rgpr, ids, idr, irhom, c, b, genss, ngenss, nggenss, 
          gensr, ngensr, nggensr, images, r, u, v, q, pq, rayrq, j, rayuv, 
          g, i, y, invy, w, rayvw, f, p, pp, rayrp, k, rayuw, h, invf, inv; 

    if not (IsInjectiveOnObjects(iso) and IsSurjectiveOnObjects(iso)) then
        Error( "mapping not bijective on objects" );
    fi; 
    Info( InfoGroupoids, 1, "InverseGeneralMapping for groupoid isos" ); 
    src := Source( iso );
    obss := src!.objects;
    rng := Range( iso ); 
    obsr := rng!.objects;
    nobs := Length( obss );
    imobs := ImagesOfObjects( iso ); 
    L := [1..nobs]; 
    SortParallel( ShallowCopy(imobs), L ); 
    invobs := List( L, i -> obss[i] ); 
    pos1 := Position( invobs, obss[1] );
    pi := PermList(L)^-1; 
    rhom := RootGroupHomomorphism( iso ); 
    rgps := RootGroup( src ); 
    rgpr := RootGroup( rng );
    ids := Arrow( src, One( rgps ), obss[1], obss[1] ); 
    idr := Arrow( rng, One( rgpr ), obsr[1], obsr[1] ); 
    if not IsBijective( rhom ) then 
        Error( "root group homomorphism has no inverse" ); 
    fi; 
    irhom := InverseGeneralMapping( rhom ); 
    genss := GeneratorsOfGroupoid( src );
    ngenss := Length( genss ); 
    nggenss := ngenss - nobs + 1;
    gensr := GeneratorsOfGroupoid( rng ); 
    ngensr := Length( gensr );
    nggensr := ngensr - nobs + 1;
    images := [1..ngensr]; 
    r := obss[1]; 
    u := imobs[1];
    v := obsr[1]; 
    q := invobs[1]; 
    pq := Position( obss, q ); 
    if ( pq = 1 ) then 
        rayrq := ids;  
    else 
        rayrq := genss[ nggenss + pq - 1 ]; 
    fi; 
    j := rayrq![2];
    rayuv := ImageElm( iso, rayrq );
    g := rayuv![2]; 
    for i in [1..nggensr] do 
        y := gensr[i]![2]; 
        invy := ImageElm( irhom, y^(g^-1) )^j; 
        images[i] := Arrow( src, invy, q, q ); 
    od; 
    for i in [2..nobs] do 
        w := obsr[i];
        rayvw := gensr[nggensr+i-1]; 
        f := rayvw![2]; 
        p := invobs[i]; 
        pp := Position( obss, p );
        if ( pp = 1 ) then 
            rayrp := ids; 
        else 
            rayrp := genss[ nggenss + pp - 1 ];
        fi; 
        k := rayrp![2]; 
        rayuw := ImageElm( iso, rayrp ); 
        h := rayuw![2]; 
        invf := (j^-1) * ImageElm( irhom, g*f*h^-1 ) * k;
        images[ nggensr + i -1 ] := Arrow( src, invf, q, p ); 
    od; 
    inv := GroupoidHomomorphism( rng, src, gensr, images );
    SetIsInjectiveOnObjects( inv, true );
    SetIsSurjectiveOnObjects( inv, true );
    SetInverseGeneralMapping( iso, inv );
    SetInverseGeneralMapping( inv, iso );
    return inv;
end );

InstallMethod( InverseGeneralMapping, "for gpd auto by object perm", true,
    [ IsGroupoidAutomorphismByObjectPerm ], 0,
function( aut )

    local gpd, obs, nobs, imobs, L, invobs, inv; 

    Info( InfoGroupoids, 1, "InverseGeneralMapping for object perm auto" );
    gpd := Source( aut );
    obs := gpd!.objects;
    nobs := Length( obs );
    imobs := ImagesOfObjects( aut ); 
    L := [1..nobs]; 
    SortParallel( ShallowCopy(imobs), L ); 
    invobs := List( L, i -> obs[i] ); 
    inv := GroupoidAutomorphismByObjectPerm( gpd, invobs ); 
    SetIsEndomorphismWithObjects( inv, true );
    SetIsInjectiveOnObjects( inv, true );
    SetIsSurjectiveOnObjects( inv, true );
    SetInverseGeneralMapping( aut, inv );
    SetInverseGeneralMapping( inv, aut );
    return inv;
end );

InstallMethod( InverseGeneralMapping, "for gpd auto by group auto", true,
    [ IsGroupoidAutomorphismByGroupAuto ], 0,
function( aut )

    local gpd, rhom, irhom, ok, inv; 

    Info( InfoGroupoids, 1, "InverseGeneralMapping for group auto auto" );
    gpd := Source( aut );
    rhom := RootGroupHomomorphism( aut ); 
    irhom := InverseGeneralMapping( rhom );
    ok := IsGroupHomomorphism( irhom );
    inv := GroupoidAutomorphismByGroupAuto( gpd, irhom ); 
    SetIsEndomorphismWithObjects( inv, true );
    SetIsInjectiveOnObjects( inv, true );
    SetIsSurjectiveOnObjects( inv, true );
    SetInverseGeneralMapping( aut, inv );
    SetInverseGeneralMapping( inv, aut );
    return inv;
end );

InstallMethod( InverseGeneralMapping, "for gpd auto by ray shifts", true,
    [ IsGroupoidAutomorphismByRayShifts ], 0,
function( aut )

    local gpd, r, len, s, h, inv; 

    Info( InfoGroupoids, 1, "InverseGeneralMapping for ray shifts auto" );
    gpd := Source( aut );
    r := RaysOfGroupoid( gpd );
    len := Length( r );
    s := ImageElementsOfRays( aut );
    h := List( [1..len], i -> s[i]^-1 * r[i] ); 
    inv := GroupoidAutomorphismByRayShifts( gpd, h ); 
    SetIsEndomorphismWithObjects( inv, true );
    SetIsInjectiveOnObjects( inv, true );
    SetIsSurjectiveOnObjects( inv, true );
    SetInverseGeneralMapping( aut, inv );
    SetInverseGeneralMapping( inv, aut );
    return inv;
end );

InstallMethod( InverseGeneralMapping, "for hom from discrete gpd with objects", 
    true, [ IsGroupWithObjectsHomomorphism and 
            IsGroupoidHomomorphismFromHomogeneousDiscrete ], 0,
function( map )

    local src, rng, obs, oims1, oims2, homs, ihoms, inv; 

    if not IsBijectiveOnObjects( map ) then 
        Error( "expecting map to be bijective on objects" ); 
    fi; 
    Info( InfoGroupoids, 3, 
          "InverseGeneralMapping for hom from discrete groupoid" );
    src := Source( map ); 
    rng := Range( map ); 
    if not ( IsHomogeneousDomainWithObjects( rng ) and 
             IsDiscreteDomainWithObjects( rng ) ) then 
        Error( "expecting range to be homogeneous discrete" ); 
    fi; 
    obs := ShallowCopy( src!.objects ); 
    oims1 := ShallowCopy( ImagesOfObjects( map ) ); 
    oims2 := ShallowCopy( oims1 ); 
    homs := ShallowCopy( ObjectHomomorphisms( map ) ); 
    SortParallel( oims1, obs ); 
    SortParallel( oims2, homs ); 
    ihoms := List( homs, InverseGeneralMapping );
    inv := GroupoidHomomorphismFromHomogeneousDiscrete( src,rng,ihoms,obs ); 
    SetInverseGeneralMapping( map, inv );
    SetInverseGeneralMapping( inv, map );
    return inv;
end ); 

#############################################################################
##
#M  \=( <m1>, <m2> )  . . . . . . . . . . . .  equality of two magma mappings
##
InstallMethod( \=, "for 2 single piece mappings", true,
    [ IsHomomorphismToSinglePiece, IsHomomorphismToSinglePiece ], 0,
function( m1, m2 )
    Info( InfoGroupoids, 4, 
          "\\= for IsHomomorphismToSinglePiece in mwohom.gi" ); 
    return ( ( Source( m1 ) =  Source( m2 ) ) and
             ( Range( m1 ) = Range( m2 ) ) and 
             ( MappingToSinglePieceData( m1 ) 
               = MappingToSinglePieceData( m2 ) ) );
end );

InstallMethod( \=, "for 2 magma mappings", true,
    [ IsMappingWithPiecesRep, IsMappingWithPiecesRep ], 0,
function( m1, m2 )

    local c1, c2, nc, src1, src2, rng1, rng2, j, s, spos, r, rpos, L, ok;

    Info( InfoGroupoids, 4, "\\= for IsMappingWithPiecesRep in mwohom.gi" ); 
    c1 := PiecesOfMapping( m1 );
    c2 := PiecesOfMapping( m2 );
    nc := Length( c1 );
    if ( nc <> Length( c2 ) ) then
        return false;
    fi;
    src1 := List( c1, Source );
    src2 := List( c2, Source );
    rng1 := List( c1, Range );
    rng2 := List( c2, Range );
    L := ListWithIdenticalEntries( nc, 0 );
    for j in [1..nc] do
        s := src1[j];
        spos := Position( src2, s );
        r := rng1[j];
        rpos := Position( rng2, r );
        if ( ( spos = fail ) or ( rpos = fail ) or ( spos <> rpos ) ) then
            return false;
        fi;
        ok := ( c1[j] = c2[spos] );
        if ( ok = false ) then
            return false;
        fi;
        L[j] := spos;
    od;
    Sort( L );
    return ( L = Set(L) );
end );

InstallMethod( \=, "for 2 connected groupoid mappings", true,
    [ IsDefaultGroupoidHomomorphismRep, 
      IsDefaultGroupoidHomomorphismRep ], 0,
function( m1, m2 ) 
    Info( InfoGroupoids, 4, 
          "\\= for IsDefaultGroupoidHomomorphismRep in gpdhom.gi" ); 
    if not ( ( Source( m1 ) =  Source( m2 ) ) 
            and ( Range( m1 ) = Range( m2 ) ) ) then 
        return false; 
    fi; 
    if not ( ImagesOfObjects( m1 ) = ImagesOfObjects( m2 ) ) then 
        return false; 
    fi; 
    if ( HasIsGeneralMappingToSinglePiece( m1 ) 
        and IsGeneralMappingToSinglePiece( m1 ) ) then  
        return ( ( RootGroupHomomorphism( m1 ) = RootGroupHomomorphism( m2 ) ) 
             and ( ImageElementsOfRays( m1 ) = ImageElementsOfRays( m2 ) ) ); 
    else 
        return fail; 
    fi; 
end );

InstallMethod( \=, "for 2 mappings of homogeneous discrete groupoids", true,
    [ IsGroupoidHomomorphismFromHomogeneousDiscreteRep, 
      IsGroupoidHomomorphismFromHomogeneousDiscreteRep ], 0,
function( m1, m2 )
    Info( InfoGroupoids, 4, 
          "\\= for discrete homogensous groupoids in mwohom.gi" ); 
    return ( ( Source( m1 ) =  Source( m2 ) ) and
             ( Range( m1 ) = Range( m2 ) ) and 
             ( ImagesOfObjects( m1 ) = ImagesOfObjects( m2 ) ) and 
             ( ObjectHomomorphisms( m1 ) = ObjectHomomorphisms( m2 ) ) );
end );

#############################################################################
##
#M  \*( <m1>, <m2> )  . . . . . .  product of two magma with objects mappings 
##
InstallMethod( \*, "for 2 magma mappings", true,
    [ IsMagmaWithObjectsHomomorphism, IsMagmaWithObjectsHomomorphism ], 0,
function( m1, m2 )

    local src1, rng1, src2, rng2, pcs1, pcr1, pcs2, pcr2, pcm1, len1, 
          pcm2, len2, pieces, i, pi, oi, found, j; 

    ## general * implemented 12/01/11 
    src1 := Source( m1 ); 
    rng1 := Range( m1 );
    src2 := Source( m2 );
    rng2 := Range( m2 ); 
    if not IsSubdomainWithObjects( src2, rng1 ) then
        Error( "Range(m1) not a submagma of Source(m2)" );
    fi;
    pcs1 := Pieces( src1 ); 
    pcr1 := Pieces( rng1 ); 
    pcs2 := Pieces( src2 ); 
    pcr2 := Pieces( rng2 ); 
    pcm1 := PiecesOfMapping( m1 );
    len1 := Length( pcm1 );
    pcm2 := PiecesOfMapping( m2 ); 
    len2 := Length( pcm2 ); 
    pieces := ListWithIdenticalEntries( len1, 0 ); 
    for i in [1..len1] do 
        pi := pcr1[i]; 
        oi := pi!.objects; 
        found := false; 
        j := 0; 
        while ( ( found = false ) and ( j < len2 ) ) do 
            j := j+1; 
            if ( oi = pcs2[j]!.objects ) then 
                found := true; 
                pieces[i] := pcm1[i] * pcm2[j]; 
            fi; 
        od; 
        if ( found = false ) then 
            Error( "composite piece not found" ); 
        fi; 
    od; 
    return HomomorphismByUnion( src1, rng2, pieces );
end );

InstallMethod( \*, "for 2 single piece magma mappings", true,
    [ IsMagmaWithObjectsHomomorphism, IsHomomorphismToSinglePiece ], 0,
function( map1, map2 )

    local src1, rng1, src2, rng2, pos, pieces1, len1, i, m, maps, 
          images, imo1, hom1, s1, ob1, nob1, imo2, hom2, s2, ob2, nob2,
          imo12, j, o, p, hom;

    Info( InfoGroupoids, 3, "second method for * with map2 single piece" );
    src1 := Source( map1 );
    rng1 := Range( map1 );
    src2 := Source( map2 );
    rng2 := Range( map2 ); 
    if not IsSubdomainWithObjects( src2, rng1 ) then
        Error( "Range(map1) not a submagma of Source(map2)" );
    fi;
    pieces1 := PiecesOfMapping( map1 );
    len1 := Length( pieces1 );
    maps := ListWithIdenticalEntries( len1, 0 );
    images := MappingToSinglePieceData( map2 );
    for i in [1..len1] do
        m := pieces1[i];
        if not IsSinglePiece( Source(m) ) then
            Print( "general * not yet implemented\n" );
            return fail;
        fi;
        imo1 := MappingToSinglePieceData(m)[1][2];
        hom1 := MappingToSinglePieceData(m)[1][1];
        s1 := Source( m );
        ob1 := s1!.objects;
        nob1 := Length( ob1 );
        pos := Position( Pieces( src2 ), Range( m ) );
        s2 := Pieces( src2 )[pos];
        ob2 := s2!.objects;
        nob2 := Length( ob2 );
        imo2 := images[pos][2];
        hom2 := images[pos][1];
        imo12 := ListWithIdenticalEntries( nob1, 0 );
        for j in [1..nob1] do
            o := imo1[j];
            p := Position( ob2, o );
            imo12[j] := imo2[p];
        od;
        hom := hom1 * hom2;
        maps[i] := HomomorphismToSinglePiece( s1, rng2, [ [imo12,hom] ] );
    od;
    return HomomorphismByUnion( src1, rng2, maps );
end );

InstallMethod( \*, "for two single piece magma mappings", true,
    [ IsHomomorphismToSinglePiece, IsHomomorphismToSinglePiece ], 0,
function( m1, m2 )

    local s1, r1, s2, r2, oi1, ob1, nob1, ob2, oi2, oi12, j, o, p, hom, 
          gens, images;

    Info( InfoGroupoids, 3, 
          "third method for * with map1 & map2 single piece" );
    s1 := Source( m1 );
    if not IsSinglePiece( s1 ) then 
        Error("source of m1 not single piece - general method not installed");
    fi;
    r1 := Range( m1 );
    s2 := Source( m2 );
    r2 := Range( m2 );
    if not IsSubdomainWithObjects( s2, r1 ) then
        Error( "Range(m1) not a submagma of Source(m2)" );
    fi;
    oi1 := MappingToSinglePieceData( m1 )[1];
    ob1 := s1!.objects;
    nob1 := Length( ob1 );
    ob2 := s2!.objects;
    oi2 := MappingToSinglePieceData( m2 )[1];
    oi12 := ListWithIdenticalEntries( nob1, 0 );
    for j in [1..nob1] do
        o := oi1[2][j];
        p := Position( ob2, o );
        oi12[j] := oi2[2][p];
    od;
    hom := oi1[1] * oi2[1];
    if IsGroupoid( s1 ) and IsSinglePiece( s1 ) then
        gens := GeneratorsOfGroupoid( s1 );
        images := List( gens, g -> ImageElm( m1, g ) );
        images := List( images, g -> ImageElm( m2, g ) );
        return GroupoidHomomorphismFromSinglePiece( s1, r2, gens, images ); 
    else
        return HomomorphismToSinglePiece( s1, r2, [ [ hom, oi12 ] ] );
    fi;
end );

InstallMethod( \*, "for 2 connected groupoid homomorphisms", true,
    [ IsGroupoidHomomorphism and IsDefaultGroupoidHomomorphismRep, 
      IsGroupoidHomomorphism and IsDefaultGroupoidHomomorphismRep ], 0,
function( m1, m2 )

    local s1, r1, s2, r2, mgi1, img1, img2;

    Info( InfoGroupoids, 2, 
          "method 1 for * with IsDefaultGroupoidHomomorphismRep" );
    s1 := Source( m1 ); 
    r1 := Range( m1 );
    s2 := Source( m2 );
    r2 := Range( m2 );
    if not IsSubdomainWithObjects( s2, r1 ) then
        Error( "Range(m1) not a subgroupoid of Source(m2)" );
    fi; 
    mgi1 := MappingGeneratorsImages( m1 ); 
    img1 := mgi1[2]; 
    img2 := List( img1, a -> ImageElm( m2, a ) ); 
    return GroupoidHomomorphismFromSinglePieceNC( s1, r2, mgi1[1], img2 );
end );

InstallMethod( \*, "for maps from hom discrete groupoids", true,
    [ IsGroupoidHomomorphismFromHomogeneousDiscreteRep, 
      IsGroupoidHomomorphismFromHomogeneousDiscreteRep ], 0,
function( m1, m2 )

    local s1, r1, s2, r2, ob1, nob1, ob2, io1, io2, 
          homs, oims, hom1, hom2, pos, j, m;

    Info( InfoGroupoids, 3, "method 2 for * with maps from hom discrete gpds" );
    s1 := Source( m1 ); 
    r1 := Range( m1 );
    s2 := Source( m2 );
    r2 := Range( m2 );
    if not IsSubdomainWithObjects( s2, r1 ) then
        Error( "Range(m1) not a subgroupoid of Source(m2)" );
    fi; 
    ob1 := s1!.objects;
    nob1 := Length( ob1 );
    ob2 := s2!.objects; 
    io1 := ImagesOfObjects( m1 ); 
    io2 := ImagesOfObjects( m2 ); 
    oims := [1..nob1]; 
    homs := [1..nob1]; 
    hom1 := ObjectHomomorphisms( m1 ); 
    hom2 := ObjectHomomorphisms( m2 ); 
    for j in [1..nob1] do 
        pos := Position( ob2, io1[j] ); 
        oims[j] := io2[ pos ]; 
        homs[j] := hom1[j] * hom2[pos]; 
    od; 
    m := GroupoidHomomorphismFromHomogeneousDiscreteNC( s1, r2, homs, oims ); 
    SetIsGroupWithObjectsHomomorphism( m, true );
    return m; 
end );

InstallMethod( \*, "for map from hom discrete with any gpd hom", true,
    [ IsGroupoidHomomorphismFromHomogeneousDiscreteRep, 
      IsGroupoidHomomorphism and IsDefaultGroupoidHomomorphismRep ], 0,
function( m1, m2 )

    local s1, r1, s2, r2, ob1, nob1, ob2, io1, io2, homs, oims, hom1, j;

    Info( InfoGroupoids, 2, 
          "method 3 for * with first a map from hom discrete" );
    s1 := Source( m1 ); 
    r1 := Range( m1 );
    s2 := Source( m2 );
    r2 := Range( m2 );
    if not IsSubdomainWithObjects( s2, r1 ) then
        Error( "Range(m1) not a subgroupoid of Source(m2)" );
    fi; 
    ob1 := s1!.objects;
    nob1 := Length( ob1 );
    ob2 := s2!.objects; 
    io1 := ImagesOfObjects( m1 ); 
    io2 := ImagesOfObjects( m2 ); 
    oims := [1..nob1]; 
    homs := [1..nob1]; 
    hom1 := ObjectHomomorphisms( m1 ); 
    for j in [1..nob1] do
        oims[j] := io2[ Position( ob2, io1[j] ) ]; 
        homs[j] := hom1[j] * ObjectGroupHomomorphism( m2, io1[j] ); 
    od;
    return GroupoidHomomorphismFromHomogeneousDiscreteNC( s1, r2, homs, oims );
end );

#############################################################################
##
#M  \^( <e>, <n> )  . . . . . . . . . . power of a magma with objects mapping
##
InstallMethod( \^, "for a magma with objects mapping and an integer", true,
    [ IsMagmaWithObjectsHomomorphism, IsInt ], 0,
function( map, n )

    local i, m1, m2; 

    if not ( ( Source(map) = Range(map) ) or ( n = -1 ) ) then
        Error( "source <> range  or  n <> -1" );
    fi;
    if ( n = 1 ) then
        return map;
    elif ( n = 0 ) then
        return IdentityMapping( Source( map ) );
    elif ( n = -1 ) then
        return InverseGeneralMapping( map );
    elif ( n > 1 ) then
        m1 := map;
        for i in [2..n] do
            m2 := map * m1;
            m1 := m2;
        od;
        return m1;
    else  ## n < -1
        return InverseGeneralMapping( map )^(-n);
    fi;
    Info( InfoGroupoids, 1, "unexpected failure in \^" ); 
    return fail;
end );

#############################################################################
##
#M  Order( <m> )  . . . . . . . . . . . . . . . . . .  of a magma mapping
##
InstallMethod( Order, "for a magma mapping", true,
    [ IsMagmaWithObjectsHomomorphism ], 0,
function( map )
    Print( "Order only implemented for single piece mappings at present\n" );
    return fail;
end );

InstallMethod( Order, "for a single piece magma mapping", true,
    [ IsHomomorphismToSinglePiece and IsAutomorphismWithObjects ], 0, 
function( m )

    local im, obsrc, oblist, obord, hom, homord, ok;

    im := InverseMapping( m ); 
    if (  im = fail ) then
        Error( "inverse does not exist" );
    fi; 
    obsrc := ObjectList( Source( m ) );
    oblist := List( ImagesOfObjects( m ), o -> Position( obsrc, o ) );
    obord := Order( PermList( oblist ) );
    hom := RootGroupHomomorphism( m );
    homord := Order( hom);
    return Lcm( [ obord, homord ] );
end );

#############################################################################
##
#M  String, ViewString, PrintString, ViewObj, PrintObj 
##  . . . . . . . . . . . . . . for homomorphisms between magmas with objects 
##
InstallMethod( String, "for a magma with objects homomorphism", true, 
    [ IsMagmaWithObjectsHomomorphism ], 0, 
function( hom ) 
    if ( "IsGroupoidHomomorphism" in CategoriesOfObject(hom) ) then 
        return( STRINGIFY( "groupoid homomorphism" ) ); 
    else 
        return( STRINGIFY( "magma with objects homomorphism" ) ); 
    fi;
end );

InstallMethod( ViewString, "for a magma with objects homomorphism", true, 
    [ IsMagmaWithObjectsHomomorphism ], 0, String ); 

InstallMethod( PrintString, "for a magma with objects homomorphism", true, 
    [ IsMagmaWithObjectsHomomorphism ], 0, String ); 

InstallMethod( ViewObj, "for a magma with objects homomorphism", true, 
    [ IsMagmaWithObjectsHomomorphism ], 0, PrintObj ); 

InstallMethod( PrintObj, "for mwo homomorphism to single piece", true,
    [ IsHomomorphismToSinglePiece ], 0, 
function ( hom ) 
    local src, rng, mgi, ngens, i; 
    src := Source( hom ); 
    rng := Range( hom ); 
    if ( "IsGroupoidHomomorphism" in CategoriesOfObject(hom) ) then 
        Print( "groupoid homomorphism : " ); 
    else 
        Print( "magma with objects homomorphism : " ); 
    fi;
    if ( HasName( src ) and HasName( rng ) ) then 
        Print( src, " -> ", rng, "\n" ); 
    else 
        Print( "\n" );
    fi; 
    if ( "IsGroupoidHomomorphism" in CategoriesOfObject(hom) ) then 
        Print( MappingGeneratorsImages( hom ) ); 
    else 
        Print( MappingToSinglePieceData( hom ) ); 
    fi;
end );

InstallMethod( PrintObj, "for a general mwo mapping by function", true,
    [ IsMappingWithObjectsByFunction ], 10, 
function ( map ) 
    Print( "magma with objects mapping by function : " );  
    Print( Source( map ), " -> ", Range( map ), "\n" );  
    Print( "function: ", UnderlyingFunction( map ) );  
end );

InstallMethod( PrintObj, "for a general mwo homomorphism", true,
    [ IsMagmaWithObjectsHomomorphism ], 0, 
function ( hom ) 
    Print( "magma with objects homomorphism : " );  
    if ( HasName( Source(hom) ) and HasName( Range(hom) ) ) then 
        Print( Source(hom), " -> ", Range(hom), "\n" ); 
    fi; 
    Print( PiecesOfMapping( hom ) );  
end );

InstallMethod( PrintObj, "for a groupoid homomorphism", true,
    [ IsGroupoidHomomorphism ], 0, 
function ( hom ) 

    local h; 

    if HasPiecesOfMapping( hom ) then 
        Print( "groupoid homomorphism from several pieces : \n" );
        for h in PiecesOfMapping( hom ) do 
            Print( h, "\n" ); 
        od; 
    else 
        Print( "groupoid homomorphism : " );
        if ( HasName( Source(hom) ) and HasName( Range(hom) ) ) then 
            Print( Source(hom), " -> ", Range(hom), "\n" ); 
        elif IsGroupoidHomomorphismFromHomogeneousDiscrete( hom ) then 
            Print( "morphism from a homogeneous discrete groupoid:\n" ); 
            Print( ObjectList(Source(hom)), " -> ", 
                   ImagesOfObjects(hom), "\n" ); 
            Print( "object homomorphisms:\n" );  
            for h in ObjectHomomorphisms( hom ) do 
                Print( h, "\n" ); 
            od; 
        else 
            Print( MappingToSinglePieceData( hom ) ); 
        fi; 
    fi;
end ); 

#############################################################################
##
#M  Display
##
InstallMethod( Display, "for a homomorphsim to a single piece magma", true,
    [ IsHomomorphismToSinglePiece ], 0,
function ( hom )

    local homs, imo, src, rng, isgpd, pieces, i, c, maps, map1, mgi, len;

    Info( InfoGroupoids, 2, "first display method for homs in mwohom.gi" );
    src := Source( hom );
    rng := Range( hom );
    isgpd := "IsGroupoid" in CategoriesOfObject(rng); 
    if isgpd then 
        Print( "homomorphism to single piece groupoid" );
    else 
        Print( "homomorphism to single piece magma" );
    fi;
    imo := ImagesOfObjects( hom ); 
    if HasMappingToSinglePieceMaps( hom ) then 
        maps := MappingToSinglePieceMaps( hom ); 
    else 
        maps := MappingToSinglePieceData( hom );
    fi;
    len := Length( maps );
    if ( len = 1 ) then 
        if ( HasName( src ) and HasName( rng ) ) then 
            Print( ": ", src, " -> ", rng, "\n" ); 
        else 
            Print( ":\n" );
            Print( "[ ", src, " ] -> [ ", rng, " ]\n" );
        fi; 
        map1 := maps[1]; 
        if isgpd then 
            Print( "root group homomorphism:\n" );
            mgi := MappingGeneratorsImages( map1[1] );
            for i in [1..Length( mgi[1] )] do
                Print( mgi[1][i], " -> " );
                Display( mgi[2][i] );
            od;
        else 
            Print( " magma hom: " ); 
            Display( map1[1] ); 
        fi;
        Print( "object map: ", src!.objects, " -> ", imo, "\n"); 
        if ( Length( map1 ) > 2 ) then 
            Print( "ray images: ", map1[3], "\n" );
        fi; 
    else
        Print( " with mappings:\n" );
        for i in [1..len] do
            Print( "(", i, ") : " ); 
            Display( maps[i] );
        od;
    fi;
end );

InstallMethod( Display, "generic method for a magma mapping", true,
    [ IsGeneralMappingWithObjects ], 0,
function ( hom )

    local src, rng, isgpd, chom, pieces, i, c, maps, len;

    Info( InfoGroupoids, 2, "second display method for homs in mwohom.gi" );
    src := Source( hom );
    rng := Range( hom );
    isgpd := ( "IsGroupoid" in CategoriesOfObject( src ) ) and
             ( "IsGroupoid" in CategoriesOfObject( rng ) );   
    if isgpd then 
        Print( "groupoid homomorphism: " );
    else
        Print( "magma homomorphism: " );
    fi;
    Print( Source( hom ), " -> ",  Range( hom ), " with pieces :\n" );
    for chom in PiecesOfMapping( hom ) do
        src := Source( chom );
        rng := Range( chom );
        if HasMappingToSinglePieceMaps( chom ) then 
            maps := MappingToSinglePieceMaps( chom ); 
        else 
            maps := MappingToSinglePieceData( chom );
        fi;
        len := Length( maps );
        if ( len = 1 ) then
            Display( maps[1] ); 
        else 
            for i in [1..len] do 
                Print( "(", i, ") : " ); 
                Display( maps[i] ); 
            od;
        fi;
    od;
end );

#############################################################################
##
#M  ImageElm( <map>, <e> )
##
InstallOtherMethod( ImageElm, "for a magma with objects hom", true,
   [ IsHomomorphismToSinglePiece, IsMultiplicativeElementWithObjects ], 0,
function ( map, e )

    local G1, ce, C1, pe, Ge, obs1, hom, obs2, mag2, t2, h2, g2;

    #?  need to include some tests here ?? 
    Info( InfoGroupoids, 3, 
          "this is the first ImageElm function in mwohom.gi" ); 
    G1 := Source( map );
    ce := PieceOfObject( G1, e![3] );
    C1 := Pieces( G1 );
    pe := Position( C1, ce );
    Ge := C1[pe];
    obs1 := Ge!.objects;
    hom := MappingToSinglePieceData( map )[pe][1];
    obs2 := MappingToSinglePieceData( map )[pe][2]; 
    #?  this is not the correct range magma ??
    mag2 := Range( map ); 
    t2 := obs2[ Position( obs1, e![3] ) ];
    h2 := obs2[ Position( obs1, e![4] ) ]; 
    g2 := ImageElm( hom, e![2] );
    return MultiplicativeElementWithObjects( mag2, g2, t2, h2 );
end );

InstallOtherMethod( ImageElm, "for a magma with objects hom", true,
   [ IsGeneralMappingWithObjects, IsMultiplicativeElementWithObjects ], 0,
function ( map, e )

    local M1, C1, pe, obs1, pom, pim, obs2, t2, h2, g2; 

    Info( InfoGroupoids, 3, 
          "this is the second ImageElm function in mwohom.gi" ); 
    M1 := Source( map ); 
    C1 := Pieces( M1 );
    pe := Position( C1, PieceOfObject( M1, e![3] ) ); 
    obs1 := C1[pe]!.objects; 
    pom := PiecesOfMapping( map )[pe]; 
    pim := MappingToSinglePieceData( pom )[1];
    obs2 := pim[2]; 
    t2 := obs2[ Position( obs1, e![3] ) ];
    h2 := obs2[ Position( obs1, e![4] ) ]; 
    g2 := ImageElm( pim[1], e![2] );
    return MultiplicativeElementWithObjects( Range( map ), g2, t2, h2 );
end ); 

InstallOtherMethod( ImageElm, "for a magma with objects endomorphism", true,
    [ IsHomomorphismToSinglePiece and IsEndomorphismWithObjects, 
      IsMultiplicativeElementWithObjects ], 0,
function ( map, elt )

    local mag, obs, one, gensims, gens, ims, rhom, 
          e, j, ej, pj, k, ek, pk, r, im;

    Info( InfoGroupoids, 3, 
          "this is the third ImageElm function in mwohom.gi" ); 
    mag := Source( map );
    obs := mag!.objects; 
    one := One( mag!.magma );
    gensims := MappingGeneratorsImages( map )[1];
    gens := gensims[1]; 
    ims := gensims[2];
    #? (13/09/17) was: rhom := HomsOfMapping( map )[1]; 
    rhom := MappingToSinglePieceData( map )[1][1]; 
    e := ImageElm( rhom, elt![2] ); 
    j := elt![2];
    pj := Position( obs, elt![3] );
    k := elt![3];
    pk := Position( obs, elt![4] );
    r := Length( GeneratorsOfMagma( Source( rhom ) ) ); 
    ## n := Length( m!.objects ); 
    if ( pj = 1 ) then 
        ej := one;
    else
        ej := ims[r+pj-1]![2]; 
    fi;
    if ( pk = 1 ) then
        ek := one;
    else
        ek := ims[r+pk-1]![2]; 
    fi; 
    #?  MagmaElement only requires two parameters ??
    im := MagmaElement( mag, ej^(-1)*e*ek, j, k );
    return im;
end );

InstallOtherMethod( ImageElm, "for a mapping with objects by function", true,
   [ IsMappingWithObjectsByFunction, IsMultiplicativeElementWithObjects ], 10,
function ( map, e )

    local fun, fe, obs, imo, pt, ph; 

    Info( InfoGroupoids, 3, 
          "this is the fourth ImageElm function in mwohom.gi" ); 
    fun := UnderlyingFunction( map ); 
    fe := fun( e ); 
    obs := Source( map )!.objects; 
    imo := ImagesOfObjects( map );
    pt := Position( obs, e![3] ); 
    ph := Position( obs, e![4] ); 
    if not ( ( fe![3] = imo[pt] ) and ( fe![4] = imo[ph] ) ) then 
        Error( "wrong objects in ImageElm" );
    else 
        return fe; 
    fi;
end ); 

InstallOtherMethod( ImagesSource, "for a magma mapping", true, 
    [ IsHomomorphismToSinglePiece ], 0,
function ( map )

    local src, par, impar, gens, imgs, imo, hom, img, rng;

    Info( InfoGroupoids, 3, "ImagesSource for a map to a single piece" ); 
    if not IsInjectiveOnObjects( map ) then
        Error( "not yet implemented when not injective on objects" );
    fi;
    src := Source( map );
    if not IsSinglePiece( src ) then 
        Error( "not yet implemented when source not a single piece" ); 
    fi; 
    if HasParentMappingGroupoids( map ) then 
        par := ParentMappingGroupoids( map ); 
        impar := ImagesSource( par );
        gens := GeneratorsOfGroupoid( src );
        imgs := List( gens, g -> ImageElm( map, g ) ); 
        return SinglePieceSubgroupoidByGenerators( impar, imgs ); 
    fi;
    imo := ShallowCopy( ImagesOfObjects( map ) );
    Sort( imo );
    hom := MappingToSinglePieceData( map )[1][1];
    img := Image( hom );
    rng := Range( map );
    if ( ( imo = rng!.objects ) and ( img = rng!.magma ) ) then
        return rng;
    elif HasLargerDirectProductGroupoid( rng ) then 
        return SubdomainWithObjects( 
                   LargerDirectProductGroupoid( rng ), [ [ img, imo ] ] );
    else
        return SubdomainWithObjects( rng, [ [ img, imo ] ] );
    fi;
end );

InstallOtherMethod( ImagesSource, "for a magma mapping", true,
    [ IsMagmaWithObjectsHomomorphism ], 0,
function ( map )
    Error( "not yet implemented when Range(map) is not single piece" );
    Info( InfoGroupoids, 3, "ImagesSource for a map to more than one piece" ); 
end );

#############################################################################
##
#M  IsEndoGeneralMapping( <map> )
##
InstallMethod( IsEndoGeneralMapping, "for a mwo mapping", true,
  [ IsMagmaWithObjectsHomomorphism ], 0,
function ( map ) 

    local obs; 

    obs := Source( map )!.objects; 
    return ForAll( MappingToSinglePieceData( map ), 
        c -> ( IsEndoGeneralMapping( c[1] ) and IsSubset( obs, c[2] ) ) ); 
end ); 
