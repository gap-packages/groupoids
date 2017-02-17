##############################################################################
##
#W  mwohom.gi                 GAP4 package `gpd'                 Chris Wensley
#W                                                                & Emma Moore
#Y  Copyright (C) 2000-2017, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  This file contains generic methods for mappings of magmas with objects
##

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

    local  nargs, id, rays;
    nargs := Length( arg );

    if ( ( nargs < 3 ) or not IsMagmaWithObjects( arg[1] ) or 
         ( nargs > 4 ) or not IsMagmaWithObjects( arg[2] ) ) then 
        Info( InfoGpd, 1, MAGMA_HOMOMORPHISM_CONSTRUCTORS );
        return fail;
    fi;
    # mwo, mwo, magma mapping, object images
    if ( ( nargs = 4 ) and IsSinglePiece( arg[1] ) 
                       and IsMagmaHomomorphism( arg[3] ) 
                       and IsHomogeneousList( arg[4] ) ) then 
        Info( InfoGpd, 2, "HomomorphismFromSinglePiece" );
        return HomomorphismFromSinglePiece(arg[1],arg[2],arg[3],arg[4]); 
    # mwo, mwo, list of mappings
    elif ( ( nargs = 3 ) and IsHomogeneousList( arg[3] ) 
            and IsMagmaWithObjectsHomomorphism( arg[3][1] ) ) then
        Info( InfoGpd, 2, "HomomorphismByUnion" );
        return HomomorphismByUnion( arg[1], arg[2], arg[3] );
    # mwo, mwo, list of [mapping,[images of objects]] pairs
    elif ( ( nargs = 3 ) and IsHomogeneousList( arg[3] ) 
           and IsList( arg[3][1] ) and IsList( arg[3][1][1] ) ) then 
        Info( InfoGpd, 2, "HomomorphismToSinglePiece" );
        return HomomorphismToSinglePiece( arg[1], arg[2], arg[3] );
    else
        Info( InfoGpd, 1, MAGMA_HOMOMORPHISM_CONSTRUCTORS );
        return fail;
    fi;
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
    #?  (17/11/08)  put separate code here ?? 
    return HomomorphismToSinglePieceNC( m1, m2, [ [ hom, imo ] ] );
end );

InstallMethod( HomomorphismFromSinglePiece,
    "generic method for a mapping of connected magmas", true,
    [ IsSinglePiece, IsSinglePiece, IsMagmaHomomorphism, IsHomogeneousList ], 
    0,
function( m1, m2, hom, imo )

    local  o1, o2, no2, mag1, mag2;

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

    local  fam, isgpdhom, filter, map, ok, imo, homs;

    #? (23/04/10) removed:  fam := FamilyObj( [ mag1, mag2, images ] ); 
    fam := GeneralMappingWithObjectsFamily; 
    isgpdhom := ( IsGroupoid( mag1 ) and IsGroupoid( mag2 ) ); 
    ## (22/01/13) added first filter in this test
    if isgpdhom then 
        filter := IsMappingToSinglePieceRep and IsGroupoidHomomorphism; 
    else 
        filter := IsMappingToSinglePieceRep and IsMagmaWithObjectsHomomorphism;
    fi; 
    map := rec(); 
    ObjectifyWithAttributes( map, NewType( fam, filter ), 
        Source, mag1, 
        Range, mag2, 
        PieceImages, images,  
        RespectsMultiplication, true, 
        IsHomomorphismToSinglePiece, true );
    ok := IsInjectiveOnObjects( map ); 
    ok := IsSurjectiveOnObjects( map ); 
    if not isgpdhom then 
        if ( IsMagmaWithObjectsAndOnes( mag1 )   
               and IsMagmaWithObjectsAndOnes( mag2 ) ) then 
            SetIsMonoidWithObjectsHomomorphism( map, true ); 
        elif ( ( HasIsSemigroupWithObjects( mag1 ) 
                 and IsSemigroupWithObjects( mag1 ) )  
               and ( HasIsSemigroupWithObjects( mag2 ) 
                     and IsSemigroupWithObjects( mag2 ) ) ) then 
            SetIsSemigroupWithObjectsHomomorphism( map, true );
        elif ( IsMagmaWithObjects( mag1 ) and IsMagmaWithObjects( mag2 ) ) then 
            SetIsMagmaWithObjectsHomomorphism( map, true );
        fi; 
    fi; 
    ok := IsHomomorphismFromSinglePiece( map ); 
##    if ( HasIsSinglePiece( mag1 ) and IsSinglePiece( mag1 ) ) then 
##        SetIsGeneralMappingFromSinglePiece( map, true ); 
##    fi; 
    return map; 
end );

InstallMethod( HomomorphismToSinglePiece,
    "generic method for a mapping to a single piece magma", true,
    [ IsMagmaWithObjects, IsSinglePiece, IsHomogeneousList ], 0,
function( mwo1, mwo2, images )

    local  pieces, o2, m2, j, pj, h;

    Info( InfoGpd, 3, "homomorphism to a single piece magma:", images ); 
    pieces := Pieces( mwo1 ); 
    o2 := mwo2!.objects;
    m2 := mwo2!.magma;
    for j in [1..Length(pieces)] do 
        pj := pieces[j];
        h := images[j][1];
        if not IsGeneralMapping( h ) then
            Error( "images[[j][1] is not a magma mapping" );
        fi;
        if not ( ( Source(h) = pj!.magma ) and ( Range(h) = m2 ) ) then
            Error( "homs not a mapping m1!.magma -> m2!.magma" );
        fi;
        if not ( Length( images[j][2] ) = Length( pj!.objects ) ) then
            Error( "incorrect length for images of objects" );
        fi;
        if not IsSubset( o2, images[j][2] ) then
            Error( "object images not in the objects of m2" );
        fi;
    od;
    return HomomorphismToSinglePieceNC( mwo1, mwo2, images );
end );

#############################################################################
##
#M  HomomorphismByUnionNC  
#M  HomomorphismByUnion
##
InstallMethod( HomomorphismByUnionNC, 
    "generic method for a magma mapping", true,
    [ IsMagmaWithObjects, IsMagmaWithObjects, IsList ], 0,  
function( mag1, mag2, maps )

    local  fam, filter, map, inj, pieces1, nc1, pieces2, nc2, obs1, 
           part, i, m, src;

    if IsSinglePiece( mag2 ) then 
        Info( InfoGpd, 1, "better to use single piece function" );
        #?  (17/11/08)  fix it so the following works ?? 
        ## return HomomorphismToSinglePiece( mag1, mag2, maps );
    fi; 
    fam := GeneralMappingWithObjectsFamily;
    filter := IsMappingWithPiecesRep and IsMagmaWithObjectsHomomorphism;
    map := Objectify( NewType( fam, filter ), rec () );
    SetSource( map, mag1 );
    SetRange( map, mag2 );
    SetPiecesOfMapping( map, maps );
    SetIsGeneralMappingToSinglePiece( map, false ); 
    inj := IsInjectiveOnObjects( map ); 
    #? added 10/05/06 -- it is useful ?? 
    pieces1 := Pieces( mag1 );
    nc1 := Length( pieces1 );
    pieces2 := Pieces( mag2 );
    nc2 := Length( pieces2 );
    obs1 := List( pieces1, g -> g!.objects[1] ); 
    part := ListWithIdenticalEntries( nc2, 0 );
    for i in [1..nc2] do
        m := maps[i];
        src := Source( m );
        if IsSinglePiece( src ) then
            part[i] := [ Position( obs1, src!.objects[1] ) ];
        else
            part[i] := List( Pieces( src ),  
                       c -> Position( obs1, c!.objects[1] ) );
        fi;
    od;
    Info( InfoGpd, 3, "part = ", part );
    SetPartitionOfSource( map, part );
    return map; 
end );

InstallMethod( HomomorphismByUnion, 
    "generic method for a magma mapping", true,
    [ IsMagmaWithObjects, IsMagmaWithObjects, IsList ], 0, 
    #? (15/06/11) was SinglePiece: needs revising for multi-component cases? 

function( mag1, mag2, maps )

    local  pieces1, nc1, pieces2, nc2, lenmaps, lenpieces, npieces, expand, 
           src1, g, flat1, pos1, pos2, piecesmap, L, i, j, k, m, filt, 
           mapj, src;

    if not ForAll( maps, m -> IsGeneralMappingWithObjects(m) ) then 
        Error( "all maps should have IsGeneralMappingWithObjects" ); 
    fi; 
    if IsSinglePiece( mag2 ) then 
        Info( InfoGpd, 3, "using the special case in HomomorphismByUnion" ); 
        piecesmap := Concatenation( List( maps, m -> PieceImages(m) ) ); 
        return HomomorphismToSinglePieceNC( mag1, mag2, piecesmap ); 
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
    Info( InfoGpd, 3, "expanded maps:", expand );
    pos1 := ListWithIdenticalEntries( lenmaps, 0 );
    src1 := List( expand, m -> Source( m ) );
    for j in [1..lenmaps] do
        g := src1[j];
        if IsSinglePiece( g ) then
            pos1[j] := [ Position( pieces1, g ) ];
        else
            pos1[j] := List( Pieces(g),
                              c -> Position( pieces1, c ) );
        fi;
    od;
    flat1 := Flat( pos1 );
    Info( InfoGpd, 3, " pos1 = ", pos1 );
    Info( InfoGpd, 3, "flat1 = ", flat1 );
    if ( fail in flat1 ) then
        Error( "not all m have source in mag1" );
    fi;
    if not ( Set( flat1 ) = [1..Length(pieces1)] ) then
        Error( "constituents of mag1 <> union of sources in maps" );
    fi;
    ##  more efficient to use PieceNrOfObject here ??
    pos2 := List( maps, m -> Position( pieces2, 
                      PieceOfObject( mag2, Range(m)!.objects[1] ) ) );
    if ( fail in pos2 ) then
        Error( "not all m have range in mag2" );
    fi;
    if IsDuplicateFree( pos2 ) then
        ## reorder if necessary
        Info( InfoGpd, 2, "duplicate free case" );
        L := [1..nc2];
        SortParallel( pos2, L );
        piecesmap := List( L, j -> expand[j] );
        return HomomorphismByUnionNC( mag1, mag2, piecesmap );
    else
        ## construct the constituent mappings
        piecesmap := ListWithIdenticalEntries( nc2, 0 );
        for j in [1..nc2] do
            filt := Filtered( pos2, i -> (i=j) );
            mapj := List( filt, i -> maps[i] );
            if ( Length( filt ) = 1 ) then
                piecesmap[j] := mapj[1]; 
            else
                if ( Length( filt ) = nc1 ) then
                    src := mag1;
                else
                    src := UnionOfPieces( List(filt), i -> Source(maps[i]) );
                fi;
                piecesmap[j] := 
                    HomomorphismToSinglePiece( src, pieces2[j], mapj );
            fi;
        od;
        return HomomorphismByUnionNC( mag1, mag2, piecesmap );
    fi;
end );

#############################################################################
##
#M  HomomorphismFromSinglePieceGeneratorsImages 
##
InstallMethod( HomomorphismFromSinglePieceGeneratorsImages,
    "generic method for a mapping of single piece magmas", true,
    [ IsSinglePiece, IsSinglePiece, IsHomogeneousList, IsHomogeneousList ], 0,
function( m1, m2, gens, ims )

    local  obs1, nobs1, mag1, gens1, obs2, nobs2, mag2, ims2, g, map, r, rhom;

    obs1 := m1!.objects;
    nobs1 := Length( obs1 );
    obs2 := m2!.objects;
    nobs2 := Length( obs2 );
    if not ( gens = GeneratorsOfMagma( m1 ) ) then
        Error( "expecting gens = GeneratorsOfMagma( m1 )" );
    fi;
    if not ( Length( gens ) = Length( ims ) ) then
        Error( "images of incorrect length" );
    fi;
    mag1 := m1!.magma;
    gens1 := GeneratorsOfMagma( mag1 );
    mag2 := m2!.magma;
    for g in ims do 
        if not ( g![1] in mag2 ) then 
            Error( "element not in the image magma" );
        fi;
        if not ( ( g![2] in obs2 ) and ( g![3] in obs2 ) ) then 
            Error( "object not in the image objects" );
        fi;
    od;
    ## NEED MORE SOME CHECKS HERE? 
    r := Length( gens ) - nobs1 +1;
    gens1 := List( [1..r], i -> gens[i]![1] );
    ims2 := List( [1..r], i -> ims[i]![1] ); 
##  Print( "gens1, ims2 = ", gens1, ims2, "\n" ); 
##  rhom := MagmaMappingByImages( mag1, mag2, gens1, ims2 );
##  Print( "rhom = ", rhom, "\n" );
    map := HomomorphismToSinglePieceNC( m1, m2, [ [gens,ims,rhom] ] );
##  Print( "in HomomorphismFromSinglePieceGeneratorsImages:\nmap=",map,"\n" );
    SetMappingGeneratorsImages( map, [ [gens,ims] ] ); 
    return map;
end );

#############################################################################
##
#M  IsomorphismNewObjects
##
InstallMethod( IsomorphismNewObjects, "for a single piece mwo", true,
    [ IsSinglePiece, IsHomogeneousList ], 0,
function( m1, ob2 )

    local  isgpd, iso, ob1, mag, m2, id;

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

    local  isos, pieces1, nc1, i, m2, iso;

    pieces1 := Pieces( m1 );
    nc1 := Length( pieces1 );
    if not ( Length(ob2) = nc1 ) then
        Error( "new list of lists of objects has incorrect length" );
    fi;
    isos := ListWithIdenticalEntries( nc1, 0 );
    for i in [1..nc1] do 
        isos[i] := IsomorphismNewObjects( pieces1[i], ob2[i] );
    od;
    m2 := UnionOfPieces( List( isos, m -> Range(m) ) );
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

    local  pieces, gp, gens, id, len, homs, iso;

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
        pieces := List( Pieces( mwo ), p -> IdentityMapping( p ) );
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

    local  images, src, obs, ims, i, j, c, srcc, obc, imc, o, pos, 
           gensims, len, rng, obr, imobr;

    if not HasPiecesOfMapping( map ) then
        images := PieceImages( map );
        if ( Length( images ) = 1 ) then
            return images[1][2];
        else
            src := Source( map );
            obs := ObjectList( src );
            ims := ShallowCopy( obs );
            c := Pieces( src ); 
            for i in [1..Length(c)] do 
                obc := c[i]!.objects;
                imc := images[i][2];
                for j in [1..Length(obc)] do
                    pos := Position( obs, obc[j] );
                    ims[pos] := imc[j];
                od;
            od;
            return ims;
        fi;
    else  ## does have PiecesOfMapping
        src := Source( map );
        obs := ObjectList( src );
        ims := ShallowCopy( obs );
        for c in PiecesOfMapping( map ) do
            obc := Source(c)!.objects;
            for i in [1..Length(obc)] do
                pos := Position( obs, obc[i] );
                ims[pos] := ImagesOfObjects(c)[2][i];
            od;
        od;
        return ims;
    fi;
end );

########### this method now appears to be redundant ###########
InstallMethod( ImagesOfObjects, "for a magma mapping", true,
    [ IsMagmaWithObjectsHomomorphism ], 0,
function( map )

    local  src, obs, ims, i, c, obc, o, pos;

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
    return ( IsSemigroupWithObjects( Source(map) ) 
             and IsSemigroupWithObjects( Range(map) ) ); 
end );

InstallMethod( IsMonoidWithObjectsHomomorphism, "for a mapping with objects", 
    true, [ IsMagmaWithObjectsHomomorphism ], 0,
    function( map ) 
    return ( IsMagmaWithObjectsAndOnes( Source(map) ) 
             and IsMagmaWithObjectsAndOnes( Range(map) ) ); 
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

    local  images, imo;

    if HasImagesOfObjects( map ) then 
        return IsDuplicateFree( Flat( ImagesOfObjects( map ) ) );
    elif IsHomomorphismToSinglePiece( map ) then
        images := PieceImages( map );
        imo := Flat( List( images, L -> L[2] ) ); 
        return IsDuplicateFree( imo ); 
    elif ( HasIsGeneralMappingFromHomogeneousDiscrete( map ) 
           and IsGeneralMappingFromHomogeneousDiscrete( map ) ) then 
        return IsDuplicateFree( map!.oims ); 
    else  ## mapping has constituents
        imo := List( PiecesOfMapping( map ), m -> PieceImages(m)[2] );
        return IsDuplicateFree( Flat( imo ) );
    fi;
end );

InstallMethod( IsInjectiveOnObjects, "for a mapping with objects", true,
    [ IsMagmaWithObjectsHomomorphism ], 0,
function( map )
    return ForAll( PiecesOfMapping( map ), m -> IsInjectiveOnObjects( m ) ); 
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
    local  obr, images, imo;
    obr := ObjectList( Range( map ) ); 
    if HasImagesOfObjects( map ) then 
        imo := Flat( ImagesOfObjects( map ) );
    elif IsHomomorphismToSinglePiece( map ) then
        images := PieceImages( map );
        imo := Flat( List( images, L -> L[2] ) );
    else  ## mapping has constituents
        imo := Flat( List( PiecesOfMapping( map ), 
                     m -> PieceImages(m)[1][2] ) );
    fi;
    return ( Set(imo) = obr );
end );

InstallMethod( IsSurjectiveOnObjects, "for a mapping with objects", true,
    [ IsMagmaWithObjectsHomomorphism ], 0,
function( map )
    return ForAll( PiecesOfMapping( map ), m -> IsSurjectiveOnObjects( m ) ); 
end );

InstallMethod( IsSurjectiveOnObjects, 
    "for a mapping from a homogeneous, discrete groupoid", true,
    [ IsGroupoidHomomorphismFromHomogeneousDiscreteRep ], 0,
function( map ) 
    local  obr, images, imo;
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

    local  pieces, isos, inv;

    Info( InfoGpd, 3, "InverseGeneralMapping for magma mappings" );
    pieces := PiecesOfMapping( map );
    isos := List( pieces, m -> InverseGeneralMapping( m ) );
    inv := HomomorphismByUnion( Range(map), Source(map), isos );
    SetIsInjectiveOnObjects( inv, true );
    SetIsSurjectiveOnObjects( inv, true );
    return inv;
end );

InstallMethod( InverseGeneralMapping, "for a single piece mapping", true,
    [ IsMagmaWithObjectsHomomorphism and IsHomomorphismToSinglePiece ], 0,
function( map )

    local  m1, m2, ob1, ob2, nob, hom21, len, sc1, sc2, obhom1, inv;

    if not (IsInjectiveOnObjects(map) and IsSurjectiveOnObjects(map)) then
        Error( "mapping with objects not bijective" );
    fi;
    Info( InfoGpd, 3, "InverseGeneralMapping for single piece mappings" );
    m1 := Source( map );
    if not IsSinglePiece( m1 ) then
        Error( "source is not single piece" );
    fi;
    m2 := Range( map );
    ob1 := m1!.objects;
    ob2 := m2!.objects;
    nob := Length( ob1 );
    obhom1 := PieceImages( map );
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

InstallMethod( InverseGeneralMapping, "for a connected mapping", true,
    [  IsGroupoidHomomorphism and IsHomomorphismFromSinglePiece ], 0,
function( map )

    local  m1, m2, ob1, ob2, nobs, imob1, hom12, hom21, ok, imob2, sc, hs, e, 
           iro1, piro1, iro2, piro2, L, pi, i, ri, ray2, rim12, rim21, inv;

    if not (IsInjectiveOnObjects(map) and IsSurjectiveOnObjects(map)) then
        Error( "mapping not bijective on objects" );
    fi;  
    Info( InfoGpd, 3, "InverseGeneralMapping for connected mappings" );
    m1 := Source( map );
    m2 := Range( map );
    ob1 := m1!.objects;
    ob2 := m2!.objects;
    nobs := Length( ob1 );
    imob1 := ImagesOfObjects( map ); 
    L := [1..nobs]; 
    SortParallel( ShallowCopy( imob1 ), L ); 
    pi := PermList( L ); 
    imob2 := ShallowCopy( ob1 );
    sc := ShallowCopy( imob1 );
    SortParallel( sc, imob2 ); 
    iro1 := imob1[1]; 
    piro1 := Position( ob2, iro1 ); 
    iro2 := imob2[1]; 
    piro2 := Position( ob1, iro2 ); 
    hom12 := RootGroupHomomorphism( map ); 
    if not IsBijective( hom12 ) then 
        Error( "root group homomorphism has no inverse" ); 
    fi; 
    hom21 := InverseGeneralMapping( RootGroupHomomorphism( map ) ); 
    ok := IsGroupHomomorphism( hom21 ); 
    ray2 := RaysOfGroupoid( m2 ); 
    rim12 := ImageElementsOfRays( map ); 
    #? (08/07/11) using an inefficient search here, but at least using break! 
    rim21 := ListWithIdenticalEntries( nobs, 0 ); 
    for i in [1..nobs] do 
        ri := ray2[i];  
        hs := Homset( m1, iro2, imob2[i] ); 
        for e in hs do 
            if ( ri = ImageElm( map, e ) ) then 
                rim21[i] := e![1]; 
                break; 
            fi; 
        od;
    od; 
    inv := GroupoidHomomorphismFromSinglePieceNC( m2, m1, hom21, imob2, rim21 );
    SetIsInjectiveOnObjects( inv, true );
    SetIsSurjectiveOnObjects( inv, true );
    return inv;
end );

InstallMethod( InverseGeneralMapping, "for hom from discrete gpd with objects", 
    true, [ IsGroupoidHomomorphism and 
            IsGroupoidHomomorphismFromHomogeneousDiscrete ], 0,
function( map )

    local  src, rng, obs, oims1, oims2, homs, ihoms; 

    if not IsBijectiveOnObjects( map ) then 
        Error( "expecting map to be bijective on objects" ); 
    fi; 
    Info( InfoGpd, 3, "InverseGeneralMapping for hom from discrete groupoid" );
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
    ihoms := List( homs, h -> InverseGeneralMapping(h) ); 
    return GroupoidHomomorphismFromHomogeneousDiscrete( src,rng,ihoms,obs ); 
end ); 

#############################################################################
##
#M  \=( <m1>, <m2> )  . . . . . . . . . . . .  equality of two magma mappings
##
InstallMethod( \=, "for 2 single piece mappings", true,
    [ IsHomomorphismToSinglePiece, IsHomomorphismToSinglePiece ], 0,
function( m1, m2 )
    Info( InfoGpd, 4, "\\= for IsHomomorphismToSinglePiece in mwohom.gi" ); 
    return ( ( Source( m1 ) =  Source( m2 ) ) and
             ( Range( m1 ) = Range( m2 ) ) and 
             ( PieceImages( m1 ) = PieceImages( m2 ) ) );
end );

InstallMethod( \=, "for 2 magma mappings", true,
    [ IsMappingWithPiecesRep, IsMappingWithPiecesRep ], 0,
function( m1, m2 )

    local  c1, c2, nc, src1, src2, rng1, rng2, j, s, spos, r, rpos, L, ok;

    Info( InfoGpd, 4, "\\= for IsMappingWithPiecesRep in mwohom.gi" ); 
    c1 := PiecesOfMapping( m1 );
    c2 := PiecesOfMapping( m2 );
    nc := Length( c1 );
    if ( nc <> Length( c2 ) ) then
        return false;
    fi;
    src1 := List( c1, m -> Source( m ) );
    src2 := List( c2, m -> Source( m ) );
    rng1 := List( c1, m -> Range( m ) );
    rng2 := List( c2, m -> Range( m ) );
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
    Info( InfoGpd, 4, "\\= for IsDefaultGroupoidHomomorphismRep in gpdhom.gi" ); 
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

#############################################################################
##
#M  \*( <m1>, <m2> )  . . . . . .  product of two magma with objects mappings 
##
InstallMethod( \*, "for 2 magma mappings", true,
    [ IsMagmaWithObjectsHomomorphism, IsMagmaWithObjectsHomomorphism ], 0,
function( m1, m2 )

    local  src1, rng1, src2, rng2, pcs1, pcr1, pcs2, pcr2, pcm1, len1, 
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

    local  src1, rng1, src2, rng2, pos, pieces1, len1, i, m, maps, 
           images, imo1, hom1, s1, ob1, nob1, imo2, hom2, s2, ob2, nob2,
           imo12, j, o, p, hom;

    Info( InfoGpd, 3, "second method for * with map2 single piece" );
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
    images := PieceImages( map2 );
    for i in [1..len1] do
        m := pieces1[i];
        if not IsSinglePiece( Source(m) ) then
            Print( "general * not yet implemented\n" );
            return fail;
        fi;
        imo1 := PieceImages(m)[1][2];
        hom1 := PieceImages(m)[1][1];
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

    local  s1, r1, s2, r2, oi1, ob1, nob1, ob2, oi2, oi12, j, o, p, hom;

    Info( InfoGpd, 3, "third method for * with map1 & map2 single piece" );
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
    oi1 := PieceImages( m1 )[1];
    ob1 := s1!.objects;
    nob1 := Length( ob1 );
    ob2 := s2!.objects;
    oi2 := PieceImages( m2 )[1];
    oi12 := ListWithIdenticalEntries( nob1, 0 );
    for j in [1..nob1] do
        o := oi1[2][j];
        p := Position( ob2, o );
        oi12[j] := oi2[2][p];
    od;
    hom := oi1[1] * oi2[1]; 
    return HomomorphismToSinglePiece( s1, r2, [ [ hom, oi12 ] ] );
end );

InstallMethod( \*, "for 2 connected groupoid homomorphisms", true,
    [ IsGroupoidHomomorphism and IsDefaultGroupoidHomomorphismRep, 
      IsGroupoidHomomorphism and IsDefaultGroupoidHomomorphismRep ], 0,
function( m1, m2 )

    local  s1, r1, s2, r2, ob1, nob1, ob2, io1, io2, 
           j, h1, h2, mgi1, im12, hom, oims, rims;

    Info( InfoGpd, 2, "method 1 for * with IsDefaultGroupoidHomomorphismRep" );
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
    for j in [1..nob1] do
        oims[j] := io2[ Position( ob2, io1[j] ) ]; 
    od;
    h1 := RootGroupHomomorphism( m1 ); 
    mgi1 := MappingGeneratorsImages( h1 ); 
    h2 := RootGroupHomomorphism( m2 ); 
    im12 := List( mgi1[2], x -> ImageElm( h2, x ) ); 
    hom := GroupHomomorphismByImages( Source(h1), Range(h2), mgi1[1], im12 ); 
    rims := RaysOfGroupoid( s1 ); 
    rims := List( rims, r -> ImageElm( m1, r ) ); 
    rims := List( rims, r -> ImageElm( m2, r ) ); 
    rims := List( rims, r -> r![1] ); 
    return GroupoidHomomorphismFromSinglePieceNC( s1, r2, hom, oims, rims );
end );

InstallMethod( \*, "for maps from hom discrete groupoids", true,
    [ IsGroupoidHomomorphismFromHomogeneousDiscreteRep, 
      IsGroupoidHomomorphismFromHomogeneousDiscreteRep ], 0,
function( m1, m2 )

    local  s1, r1, s2, r2, ob1, nob1, ob2, io1, io2, 
           homs, oims, hom1, hom2, pos, j;

    Info( InfoGpd, 3, "method 2 for * with maps from hom discrete gpds" );
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
    return GroupoidHomomorphismFromHomogeneousDiscreteNC( s1, r2, homs, oims );
end );

InstallMethod( \*, "for map from hom discrete with any gpd hom", true,
    [ IsGroupoidHomomorphismFromHomogeneousDiscreteRep, 
      IsGroupoidHomomorphism and IsDefaultGroupoidHomomorphismRep ], 0,
function( m1, m2 )

    local  s1, r1, s2, r2, ob1, nob1, ob2, io1, io2, 
           homs, oims, hom1, j;

    Info( InfoGpd, 2, "method 3 for * with first a map from hom discrete" );
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

    local  i, m1, m2; 

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
    Info( InfoGpd, 1, "unexpected failure in \^" ); 
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
    local  im, obsrc, oblist, obord, hom, homord, ok;

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
#M  PrintObj
#M  ViewObj  . . . . . . . this defaults to PrintObj, so no methods necessary 
##
InstallMethod( PrintObj, "for mwo homomorphism to single piece", true,
    [ IsHomomorphismToSinglePiece ], 0, 
function ( hom ) 
    local src, rng; 
    src := Source( hom ); 
    rng := Range( hom ); 
    if ( "IsGroupoidHomomorphism" in CategoriesOfObject(hom) ) then 
        Print( "groupoid homomorphism : " ); 
    else 
        Print( "magma with objects homomorphism : " ); 
    fi;
    if ( HasName( src ) and HasName( rng ) ) then 
        Print( src, " -> ", rng, "\n" ); 
    fi; 
    Print( PieceImages( hom ) );  
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

    local  h; 

    Print( "groupoid homomorphism : " );
    if ( HasName( Source(hom) ) and HasName( Range(hom) ) ) then 
        Print( Source(hom), " -> ", Range(hom), "\n" ); 
    elif IsGroupoidHomomorphismFromHomogeneousDiscrete( hom ) then 
        Print( "morphism from a homogeneous discrete groupoid:\n" ); 
        Print( ObjectList(Source(hom)), " -> ", ImagesOfObjects(hom), "\n" ); 
        Print( "object homomorphisms:\n" );  
        for h in ObjectHomomorphisms( hom ) do 
            Print( h, "\n" ); 
        od; 
    else 
        Print( PieceImages( hom ) ); 
    fi; 
end ); 

#############################################################################
##
#M  Display
##
InstallMethod( Display, "for a homomorphsim to a single piece magma", true,
    [ IsHomomorphismToSinglePiece ], 0,
function ( hom )

    local  homs, imo, src, rng, pieces, i, c, images, len;

    Print( "homomorphism to single piece magma" );
    src := Source( hom );
    rng := Range( hom );
    images := PieceImages( hom );
    len := Length( images );
    if ( len = 1 ) then 
        if ( HasName( src ) and HasName( rng ) ) then 
            Print( ": ", src, " -> ", rng, "\n" ); 
        else 
            Print( ":\n" );
            Print( "[ ", src, " ] -> [ ", rng, " ]\n" );
        fi; 
        Print( " magma hom: " ); 
        Display( images[1][1] ); 
        Print( "object map: ", src!.objects, " -> ", images[1][2], "\n");
    else
        Print( " with pieces:\n" );
        pieces := Pieces( src );
        for i in [1..Length(pieces)] do
            c := pieces[i];
            Print( "(", i, "):", " [ ", c, " ] -> [ ", rng, " ]\n" );
            Print( "magma mapping: ", 
                   MappingGeneratorsImages( images[i][1] ), "\n" );
            Print( "   object map: ", c!.objects, " -> ", images[i][2], "\n");
        od;
    fi;
end );

InstallMethod( Display, "generic method for a magma mapping", true,
    [ IsGeneralMappingWithObjects ], 0,
function ( hom )

    local  chom, src, rng, pieces, i, c, images, len;

    Print( "magma homomorphism: ", Source( hom ), " -> ", 
            Range( hom ), " with pieces :\n" );
    for chom in PiecesOfMapping( hom ) do
        src := Source( chom );
        rng := Range( chom );
        images := PieceImages( chom );
        len := Length( images );
        if ( len = 1 ) then
            Print( "[ ", src, " ] -> [ ", rng, " ]\n" );
            Print( "magma homomorphism: ", images[1][1], "\n" );
            Print("   object map: ", src!.objects, " -> ", images[1][2],"\n");
        else 
            pieces := Pieces( src );
            for i in [1..Length(pieces)] do
                c := pieces[i];
                Print( "[ ", c, " ] -> [ ", rng, " ]\n" );
                Print( "magma homomorophism: ", images[i][1], "\n" );
                Print( "   object map: ", c!.objects, 
                       " -> ", images[i][2], "\n" );
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

    local  G1, ce, C1, pe, Ge, obs1, hom, obs2, mag2, t2, h2, g2;

    #?  need to include some tests here ?? 
    Info( InfoGpd, 3, "this is the first ImageElm function in mwohom.gi" ); 
    G1 := Source( map );
    ce := PieceOfObject( G1, e![2] );
    C1 := Pieces( G1 );
    pe := Position( C1, ce );
    Ge := C1[pe];
    obs1 := Ge!.objects;
    hom := PieceImages( map )[pe][1];
    obs2 := PieceImages( map )[pe][2]; 
    #?  this is not the correct range magma ??
    mag2 := Range( map ); 
    t2 := obs2[ Position( obs1, e![2] ) ];
    h2 := obs2[ Position( obs1, e![3] ) ]; 
    g2 := ImageElm( hom, e![1] );
    return MultiplicativeElementWithObjects( mag2, g2, t2, h2 );
end );

InstallOtherMethod( ImageElm, "for a magma with objects hom", true,
   [ IsGeneralMappingWithObjects, IsMultiplicativeElementWithObjects ], 0,
function ( map, e )

    local  M1, C1, pe, obs1, pom, pim, obs2, t2, h2, g2; 

    Info( InfoGpd, 3, "this is the second ImageElm function in mwohom.gi" ); 
    M1 := Source( map ); 
    C1 := Pieces( M1 );
    pe := Position( C1, PieceOfObject( M1, e![2] ) ); 
    obs1 := C1[pe]!.objects; 
    pom := PiecesOfMapping( map )[pe]; 
    pim := PieceImages( pom )[1];
    obs2 := pim[2]; 
    t2 := obs2[ Position( obs1, e![2] ) ];
    h2 := obs2[ Position( obs1, e![3] ) ]; 
    g2 := ImageElm( pim[1], e![1] );
    return MultiplicativeElementWithObjects( Range( map ), g2, t2, h2 );
end ); 

InstallOtherMethod( ImageElm, "for a magma endomorphism", true,
    [ IsHomomorphismToSinglePiece and IsEndomorphismWithObjects, 
      IsMultiplicativeElementWithObjects ], 0,
function ( map, elt )

    local  mag, obs, one, gensims, gens, ims, rhom, 
           e, j, ej, pj, k, ek, pk, r, im;

    Info( InfoGpd, 3, "this is the third ImageElm function in mwohom.gi" ); 
    mag := Source( map );
    obs := mag!.objects; 
    one := One( mag!.magma );
    gensims := MappingGeneratorsImages( map )[1];
    gens := gensims[1]; 
    ims := gensims[2];
    rhom := HomsOfMapping( map )[1]; 
    e := ImageElm( rhom, elt![1] ); 
    j := elt![2];
    pj := Position( obs, elt![2] );
    k := elt![3];
    pk := Position( obs, elt![3] );
    r := Length( GeneratorsOfMagma( Source( rhom ) ) ); 
    ## n := Length( m!.objects ); 
    if ( pj = 1 ) then 
        ej := one;
    else
        ej := ims[r+pj-1]![1]; 
    fi;
    if ( pk = 1 ) then
        ek := one;
    else
        ek := ims[r+pk-1]![1]; 
    fi; 
    #?  MagmaElement only requires two parameters ??
    im := MagmaElement( mag, ej^(-1)*e*ek, j, k );
    return im;
end );

InstallOtherMethod( ImagesSource, "for a magma mapping", true, 
    [ IsHomomorphismToSinglePiece ], 0,
function ( map )
    local  imo, hom, img, rng;

    if not IsInjectiveOnObjects( map ) then
        Error( "not yet implemented when not injective on objects" );
    fi;
    imo := ShallowCopy( ImagesOfObjects( map ) );
    Sort( imo );
    hom := PieceImages( map )[1][1];
    img := Image( hom );
    rng := Range( map );
    if ( ( imo = rng!.objects ) and ( img = rng!.magma ) ) then
        return rng;
    else
        return SubdomainWithObjects( rng, [ [ imo, img ] ] );
    fi;
end );

InstallOtherMethod( ImagesSource, "for a magma mapping", true,
    [ IsMagmaWithObjectsHomomorphism ], 0,
function ( map )
    Error( "not yet implemented when Range(map) is not single piece" );
end );

#############################################################################
##
#M  IsEndoGeneralMapping( <map> )
##
InstallMethod( IsEndoGeneralMapping, "for a mwo mapping", true,
  [ IsMagmaWithObjectsHomomorphism ], 0,
function ( map ) 
    local  obs; 
    obs := Source( map )!.objects; 
    return ForAll( PieceImages( map ), 
        c -> ( IsEndoGeneralMapping( c[1] ) and IsSubset( obs, c[2] ) ) ); 
end ); 

##############################################################################
##
#E  mwohom.gi  . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##
