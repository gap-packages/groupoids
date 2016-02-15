##############################################################################
##
#W  gpdhom.gi                 GAP4 package `gpd'                 Chris Wensley
#W                                                                & Emma Moore
##  version 1.42, 15/02/2016 
##
#Y  Copyright (C) 2000-2016, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

#############################################################################
##  Standard error messages

GROUPOID_MAPPING_CONSTRUCTORS := Concatenation(
    "The standard operations which construct a groupoid mapping are:\n", 
    "1.  GroupoidHomomorphism( src, rng, hom | src, rng, hom, oims );\n", 
    " or GroupoidHomomorphism( one of the following parameter options );\n",
    "2.  GroupoidHomomorphismFromSinglePiece( src, rng, hom, oims, rims );\n",
    "3.  HomomorphismToSinglePiece( src, rng, list of [hom,imobs,rays]'s );\n",
    "4.  HomomorphismByUnion( src, rng, list of disjoint mappings );\n", 
    "5.  GroupoidAutomorphismByGroupAuto( gpd, auto );\n", 
    "6.  GroupoidAutomorphismByObjectPerm( gpd, oims );\n", 
    "7.  GroupoidAutomorphismByRayImages( gpd, rims );\n" ); 

#############################################################################
##
#M  IsGeneratorsOfMagmaWithInverses( <homlist> )
##
InstallMethod( IsGeneratorsOfMagmaWithInverses, "for a list of groupoid maps", 
    true, [ IsGeneralMappingWithObjectsCollection ], 0,
function( homlist ) 
    Print( "#I  using IsGeneratorsOfMagmaWithInverses in gpdhom.gi\n" ); 
    return ForAll( homlist, 
        m -> ( ( Source(m) = Range(m) ) 
               and IsEndomorphismWithObjects(m) 
               and IsInjectiveOnObjects(m) 
               and IsSurjectiveOnObjects(m) ) ); 
end );
 
#############################################################################
##
#M  IdentityMapping
##
InstallOtherMethod( IdentityMapping, "for a groupoid", true, 
    [ IsGroupoid and IsSinglePiece ], 0,
function( gpd )

    local  one, rays, iso;

    Info( InfoGpd, 3, "using IdentityMapping  in gpdaut.gi" ); 
    if ( HasIsDirectProductWithCompleteGraphDomain( gpd ) 
         and IsDirectProductWithCompleteGraphDomain( gpd ) ) then 
        one := One( gpd!.magma ); 
        rays := List( gpd!.objects, o -> one ); 
    else 
        rays := gpd!.rays; 
    fi; 
    iso := GroupoidHomomorphismFromSinglePieceNC
        ( gpd, gpd, IdentityMapping(gpd!.magma), gpd!.objects, rays );
    SetIsInjectiveOnObjects( iso, true );
    SetIsSurjectiveOnObjects( iso, true );
    return iso;
end );

#############################################################################
##
##  GroupoidHomomorphism( <gpd>,<hom>|<oims>|<rays>             automorphisms 
#F  GroupoidHomomorphism( <g1>,<g2>,<hom> )                 from single piece 
#F  GroupoidHomomorphism( <g1>,<g2>,<piece images> )       single piece range 
#F  GroupoidHomomorphism( <g1>,<g2>,<maps> )                union of mappings 
#F  GroupoidHomomorphism( <g>,<auto> )             automorphism by group auto
#F  GroupoidHomomorphism( <g>,<imobs> )           automorphism by object perm
#F  GroupoidHomomorphism( <g>,<rays> )           automorphism by ray products
#?  more cases ?? 
##
InstallGlobalFunction( GroupoidHomomorphism, function( arg )

    local  nargs, id, rays;
    nargs := Length( arg );

    if ( ( nargs < 2 ) 
         or ( nargs > 5 )  
         or not IsMagmaWithObjects( arg[1] ) 
         or ( ( nargs > 2 ) and not IsMagmaWithObjects( arg[2] ) ) 
       ) then Info( InfoGpd, 1, GROUPOID_MAPPING_CONSTRUCTORS );
              return fail;
    fi; 
    # various types of automorphism 
    if ( ( nargs = 2 ) and IsSinglePiece( arg[1] ) ) then 
        if IsGroupHomomorphism( arg[2] ) then 
            return GroupoidAutomorphismByGroupAuto( arg[1], arg[2] ); 
        elif ( IsHomogeneousList( arg[2] ) 
               and ( arg[2][1] in arg[1]!.objects ) ) then 
            return GroupoidAutomorphismByObjectPerm( arg[1], arg[2] ); 
        else 
            return GroupoidAutomorphismByRayImages( arg[1], arg[2] ); 
        fi; 
    # gpd, gpd, group hom 
    elif ( ( nargs = 3 ) and IsSinglePiece( arg[1] ) 
           and IsSinglePiece( arg[2] ) and IsGroupHomomorphism( arg[3] ) ) then 
        return GroupoidHomomorphismFromSinglePieceNC( arg[1], arg[2], arg[3], 
                   arg[2]!.objects, RayElementsOfGroupoid( arg[2] ) ); 
    # mwo, mwo, list of mappings
    elif ( ( nargs = 3 ) and IsHomogeneousList( arg[3] ) 
            and IsHomomorphismToSinglePiece( arg[3][1] ) ) then
        Info( InfoGpd, 2, "HomomorphismByUnion" );
        return HomomorphismByUnion( arg[1], arg[2], arg[3] );
    # gpd, gpd, list of [hom, images of objects, rays] triples
    elif ( ( nargs = 3 ) and IsHomogeneousList( arg[3] ) 
           and IsList( arg[3][1] ) and IsList( arg[3][1][1] ) ) then 
        Info( InfoGpd, 2, "HomomorphismToSinglePiece" );
        return HomomorphismToSinglePiece( arg[1], arg[2], arg[3] ); 
    elif ( ( nargs = 4 ) and IsGroupHomomorphism( arg[3] ) 
           and ( IsHomogeneousList( arg[4] ) ) ) then 
        return GroupoidHomomorphismFromSinglePieceNC( arg[1], arg[2], arg[3], 
                   arg[4], RayElementsOfGroupoid( arg[2] ) ); 
    elif ( nargs = 5 ) then 
        return GroupoidHomomorphismFromSinglePieceNC( 
                   arg[1], arg[2], arg[3], arg[4], arg[5] );
    else
        Info( InfoGpd, 1, GROUPOID_MAPPING_CONSTRUCTORS );
        return fail;
    fi;
end );

#############################################################################
##
#M  InclusionMappingGroupoids
##
InstallMethod( InclusionMappingGroupoids, "for sgpd of single piece groupoid", 
    true, [ IsGroupoid and IsSinglePiece, IsGroupoid ], 0,
function( gpd, sgpd )
    local  sobs, o1, c1, mappings, comps, m, inc, mor;

    if not IsSubgroupoid( gpd, sgpd ) then
        Error( "arg[2] is not a subgroupoid of arg[1]" );
    fi;
    if IsSinglePiece( sgpd ) then
        sobs := sgpd!.objects;
        if IsSinglePiece( gpd ) then
            Info( InfoGpd, 2, "InclusionMapping, one piece -> one piece" );
            inc := InclusionMappingGroups( gpd!.magma, sgpd!.magma );
            if ( inc = fail ) then
                Error( "magma mapping fails to include" );
            fi;
            return GroupoidHomomorphismFromSinglePieceNC( 
                       sgpd, gpd, inc, sobs, RayElementsOfGroupoid( sgpd ) );
        else
            o1 := sgpd!.objects[1];
            c1 := PieceOfObject( gpd, o1 );
            inc := InclusionMappingGroups( c1!.magma, sgpd!.magma );
            if ( inc = fail ) then
                Error( "magma mapping fails to include" );
            fi;
            mor := GroupoidHomomorphismFromSinglePieceNC( 
                       sgpd, c1, inc, sobs, RayElementsOfGroupoid( sgpd ) );
            return MagmaWithObjectsHomomorphism( sgpd, gpd, [ mor ] );
        fi;
    else
        Info( InfoGpd, 2, "InclusionMapping, source not connected" );
        mappings := List( Pieces( sgpd ),
                           c -> InclusionMappingGroupoids( gpd, c ) );
        return HomomorphismByUnion( sgpd, gpd, mappings );
    fi;
end );

InstallMethod( InclusionMappingGroupoids, "for a subgroupoid of a groupoid", 
    true, [ IsGroupoid, IsGroupoid ], 0,
function( gpd, sgpd )
    if not IsSubdomainWithObjects( gpd, sgpd ) then
        Error( "arg[2] is not a submagma of arg[1]" );
    fi;
    if not IsSinglePiece( sgpd ) then
        Error( "not yet implemented when arg[2] is not connected" );
    fi;
end );

InstallMethod( InclusionMappingGroupoids, "from hom discrete to single piece", 
    true, [ IsGroupoid and IsSinglePiece, IsHomogeneousDiscreteGroupoid ], 0,
function( gpd, sgpd )
    local  obs, inc, homs; 
    Info( InfoGpd, 2, "new InclusionMappingGroupoids method" ); 
    if not IsSubdomainWithObjects( gpd, sgpd ) then
        Error( "arg[2] is not a submagma of arg[1]" );
    fi; 
    obs := sgpd!.objects; 
    inc := InclusionMappingGroups( gpd!.magma, sgpd!.magma ); 
    homs := List( obs, o -> inc ); 
    return GroupoidHomomorphismFromHomogeneousDiscrete( 
               sgpd, gpd, homs, obs ); 
end );

#############################################################################
##
#M  RestrictedMappingGroupoids
##
InstallMethod( RestrictedMappingGroupoids, "for a groupoid mapping", true,
    [ IsGroupoidHomomorphism, IsGroupoid, IsGroupoid ], 0,
function( mor, U, V )

    local  src, rng, images, hom, imo, obsU, compU, lenU, mors, j, sj, osj, 
           pos, orj, rj, rhom;

    src := Source( mor );
    if not IsSinglePiece( src ) then
        Print( "not yet implemented for Source(mor) not connected\n" );
        return fail;
    fi;
    if not IsSinglePiece( V ) then
        Print( "not yet implemented for V not connected\n" );
        return fail;
    fi;
    rng := Range( mor );
    if not ( IsSubdomainWithObjects( src, U ) 
             and IsSubdomainWithObjects( rng, V ) ) then
        Error( "not a pair of submagmas" );
    fi;
    images := PieceImages( mor );
    imo := images[1][1];
    hom := images[1][2];
    obsU := ObjectList( U );
    compU := Pieces( U );
    lenU := Length( compU );
    mors := ListWithIdenticalEntries( lenU, 0 );
    for j in [1..lenU] do
        sj := compU[j];
        osj := sj!.objects;
        orj := List( osj, o -> imo[Position(obsU,o)] );
        rhom := GeneralRestrictedMapping( hom, sj!.magma, rng!.magma );
        if ( fail in MappingGeneratorsImages(rhom)[2] ) then
            Error( "magma mapping fails to restrict" );
        fi;
        #? (28/01/11) made this "NC" and added the rays, but not checked! 
        mors[j] := GroupoidHomomorphismFromSinglePieceNC( 
                       sj, rng, orj, rhom, RayElementsOfGroupoid( rng ) );
    od;
    return HomomorphismByUnion( U, V, mors );
end );

##  this one not checked
#?  should use GeneralRestrictedMapping
InstallMethod( RestrictedMappingGroupoids, "for a groupoid mapping", true,
    [ IsGroupoidHomomorphism, IsGroupoid, IsGroupoid ], 0,
function( mor, ssrc, srng )

    local  comp, len, rcomp, j, m;

    comp := PiecesOfMapping( mor );
    len := Length( comp );
    rcomp := ListWithIdenticalEntries( len, 0 );
    for j in [1..len] do
        rcomp[j] := RestrictedMappingGroupoids( comp[j], ssrc, srng );
    od;
    return HomomorphismByUnionNC( Source(mor), Range(mor), rcomp );
end );

#############################################################################
##
#M  InverseGeneralMapping 
##
InstallMethod( InverseGeneralMapping, "for a connected mapping", true,
    [  IsGroupoidHomomorphism and IsHomomorphismFromSinglePiece ], 0,
function( map )

    local  m1, m2, ob1, ob2, nobs, imob1, hom12, hom21, ok, imob2, sc, hs, e, 
           iro1, piro1, iro2, piro2, L, pi, i, ri, ray2, rim12, rim21, inv;

    if not (IsInjectiveOnObjects(map) and IsSurjectiveOnObjects(map)) then
        Error( "mapping not bijective on objects" );
    fi;  
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
        Error( "root homomorphism has no inverse" ); 
    fi; 
    hom21 := InverseGeneralMapping( RootGroupHomomorphism( map ) ); 
    ok := IsGroupHomomorphism( hom21 ); 
    #? are the following settings necessary ?? 
    #? SetIsGroupHomomorphism( hom21, true );
    #? SetIsTotal( hom21, true );
    #? SetRespectsMultiplication( hom21, true );
    #? SetIsInjective( hom21, true );
    #? SetIsSurjective( hom21, true ); 
    ray2 := RaysOfGroupoid( m2 ); 
    rim12 := ImagesOfRays( map ); 
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

InstallMethod( InverseGeneralMapping, "for a connected mapping", true,
    [  IsGeneralMappingFromHomogeneousDiscrete and IsBijectiveOnObjects ], 0,
function( map )

    local  src, rng, obs, oims1, oims2, homs, ihoms; 

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
#M  GroupoidHomomorphismFromDiscreteToSinglePiece
##
## InstallMethod( GroupoidHomomorphismFromDiscreteToSinglePiece,
##     "for a list of groupoid mappings with common connected range", true,
##     [ IsGroupoid and IsDiscrete, IsGroupoid and IsSinglePiece, 
##       IsHomogeneousList ], 0,  
## function( src, rng, maps )

#############################################################################
##
#M  RootGroupHomomorphism . . . . . . . for a groupoid hom from single piece 
##
InstallMethod( RootGroupHomomorphism, "for a groupoid hom from a single piece", 
    true, [ IsGroupoidHomomorphism and IsHomomorphismToSinglePiece ], 0,
function( mor )
    local  gpd1, gpd2, ob2, imobs, roh, mgi, ray, gp2, im2, hom;
    gpd2 := Range( mor ); 
    ob2 := gpd2!.objects; 
    imobs := ImagesOfObjects( mor ); 
    roh := RootGroupHomomorphism( mor ); 
    if ( imobs[1] = ob2[1] ) then  ## root maps to root 
        hom := roh; 
    elif ( HasIsDirectProductWithCompleteGraph( gpd2 ) and 
           IsDirectProductWithCompleteGraph( gpd2 ) ) then 
        hom := roh; 
    else 
        mgi := MappingGeneratorsImages( roh ); 
        ray := RayElementsOfGroupoid( gpd2 )[ Position( ob2, imobs[1] ) ]; 
        gp2 := ObjectGroup( gpd2, imobs[1] ); 
        im2 := List( mgi[2], g -> g^ray ); 
        hom := GroupHomomorphismByImages( Source( roh ), gp2, mgi[1], im2 );  
    fi; 
    return hom; 
end ); 

#############################################################################
##
#M  ObjectGroupHomomorphism . . . . .  . . . . . . . . . . for a groupoid hom 
##
InstallMethod( ObjectGroupHomomorphism, "for a groupoid hom and an object", 
    true, [ IsGroupoidHomomorphism, IsObject ], 0,
function( mor, obj ) 

    local  src, imobs, pos, obg, gens, loops, imloops, imgens, img, hom; 
    src := Source( mor ); 
    if ( HasIsGeneralMappingFromSinglePiece( mor ) 
         and IsGeneralMappingFromSinglePiece( mor ) ) then 
        imobs := ImagesOfObjects( mor ); 
        pos := Position( src!.objects, obj );
        obg := ObjectGroup( src, obj );
        gens := GeneratorsOfGroup( obg ); 
        loops := List( gens, g -> Arrow( src, g, obj, obj ) ); 
        imloops := List( loops, a -> ImageElm( mor, a ) ); 
        imgens := List( imloops, a -> a![1] ); 
        img := Group( imgens ); 
        hom := GroupHomomorphismByImages( obg, img, gens, imgens ); 
    elif IsGeneralMappingFromHomogeneousDiscrete( mor ) then 
        pos := Position( src!.objects, obj ); 
        hom := ObjectHomomorphisms( mor )[pos]; 
    elif HasPiecesOfMapping( mor ) then 
        pos := Position( Pieces( src ), obj ); 
        hom := ObjectGroupHomomorphism( PiecesOfMapping( mor )[pos], obj ); 
    else 
        Error( "this is a case not yet provided for" ); 
    fi; 
    return hom; 
end ); 

#############################################################################
##
#M  MappingPermObjectsImages . . . . . . for list of objects and their images 
#M  MappingTransObjectsImages . . . . .  for list of objects and their images 
##
InstallMethod( MappingPermObjectsImages, "for objects and their images", true, 
    [ IsList, IsList ], 0,
function( obs, ims )
    local  len, L; 
    len := Length( obs ); 
    if ( len <> Length(ims) ) then
        Error("<obs> and <ims> have different lengths");
    fi; 
    L := ShallowCopy( ims ); 
    Sort( L ); 
    if not ( L = obs ) then 
        return fail; 
    fi;
    L := List( obs, o -> Position( ims, o ) ); 
    return PermList( L ); 
end );

InstallMethod( MappingTransObjectsImages, "for objects and their images", true, 
    [ IsList, IsList ], 0,
function( obs, ims )
    local  len, L; 
    len := Length( obs ); 
    L := List( ims, o -> Position( obs, o ) ); 
    return Transformation( L ); 
end );

#############################################################################
##
#M  ObjectTransformationOfGroupoidHomomorphism . . . . for a groupoid mapping 
##
InstallMethod( ObjectTransformationOfGroupoidHomomorphism, 
    "for objects and images", true, [ IsDefaultGroupoidHomomorphismRep ], 0,
function( map )
    local  obs, ims; 
    ims := ImagesOfObjects( map );
    obs := Filtered( Range( map )!.objects, o -> o in ims ); 
#    if not ( Length(obs) = Length(ims) ) then 
#        Info( InfoGpd, 1, 
#            "ObjectTransformationOfGroupoidHomomorphism set to <fail>" );
#        return fail; 
#    fi; 
    if IsInjectiveOnObjects( map ) then 
        return MappingPermObjectsImages( obs, ims );
    else 
        return MappingTransObjectsImages( obs, ims ); 
    fi; 
end ); 

#############################################################################
##
#M  Display
##
InstallMethod( Display, "for a mapping of connected groupoids", true,
    [ IsGroupoidHomomorphism and IsHomomorphismFromSinglePiece ], 0,
function ( map ) 
    Print( " groupoid mapping: " ); 
    Print( "[ ", Source(map), " ] -> [ ", Range(map), " ]\n" ); 
    Print( "root homomorphism: ", 
           MappingGeneratorsImages( RootGroupHomomorphism(map) ), "\n" ); 
    Print( "images of objects: ", ImagesOfObjects( map ), "\n" ); 
    Print( "   images of rays: ", ImagesOfRays( map ), "\n" ); 
end );

InstallMethod( Display, "for a mapping from a homogeneous discrete gpd", true, 
    [ IsGroupoidHomomorphismFromHomogeneousDiscreteRep ], 0,
function ( map ) 
    local  h; 
    Print( " homogeneous discrete groupoid mapping: " ); 
    Print( "[ ", Source(map), " ] -> [ ", Range(map), " ]\n" ); 
    Print( "images of objects: ", ImagesOfObjects( map ), "\n" ); 
    Print( "object homomorphisms:\n" ); 
    for h in ObjectHomomorphisms( map ) do 
        Print( h, "\n" ); 
    od; 
end );

#############################################################################
##
#M  GroupoidHomomorphismFromSinglePieceNC 
#M  GroupoidHomomorphismFromSinglePiece 
##
InstallMethod( GroupoidHomomorphismFromSinglePieceNC,
    "generic method for a mapping of connected groupoids", true,
    [ IsGroupoid and IsSinglePiece, IsGroupoid and IsSinglePiece, 
      IsGroupHomomorphism, IsHomogeneousList, IsHomogeneousList ], 0,
function( src, rng, hom, oims, rims )

    local  fam, filter, map, ok, p;

    ## ?? (23/04/10)  fam := FamilyObj( [ src, rng, hom, oims, rims ] );
    fam := GroupoidHomomorphismFamily; 
    filter := IsDefaultGroupoidHomomorphismRep and IsGroupoidHomomorphism; 
    map := rec(); 
    ObjectifyWithAttributes( map, NewType( fam, filter ), 
        Source, src, 
        Range, rng, 
        RootGroupHomomorphism, hom, 
        ImagesOfObjects, oims, 
        ImagesOfRays, rims,  
        IsGeneralMappingWithObjects, true, 
        IsGroupoidHomomorphism, true, 
        IsHomomorphismToSinglePiece, true, 
        RespectsMultiplication, true ); 
    ok := IsInjectiveOnObjects( map ); 
    p := ObjectTransformationOfGroupoidHomomorphism( map ); 
    ok := IsSurjectiveOnObjects( map ); 
    SetIsHomomorphismFromSinglePiece( map, true ); 
    #? (10/10/08) do we really need this ?? 
    ## (15/06/11) added extra [ ] 
    SetPieceImages( map, [ [ hom, oims, rims ] ] );
    if ( src = rng ) then 
        SetIsEndoGeneralMapping( map, true ); 
        SetIsEndomorphismWithObjects( map, true ); 
        ## ok := IsAutomorphismWithObjects( map ); 
    fi; 
    return map; 
end );

InstallMethod( GroupoidHomomorphismFromSinglePiece,
    "generic method for a mapping of connected groupoids", true,
    [ IsGroupoid and IsSinglePiece, IsGroupoid and IsSinglePiece, 
      IsGroupHomomorphism, IsHomogeneousList, IsHomogeneousList ], 0,
function( src, rng, hom, oims, rims ) 

    local  gps, gpr, obs, obr, lens, lenr, rayr, pos1, i, posi; 

    gps := RootGroup( src ); 
    gpr := RootGroup( rng ); 
    if not ( ( Source(hom) = gps ) and ( Range(hom) = gpr ) ) then 
        Error( "hom not a map from RootGroup(src) to RootGroup(rng) " ); 
    fi; 
    obs := ObjectList( src ); 
    lens := Length( obs ); 
    obr := ObjectList( rng ); 
    lenr := Length( obr ); 
    if not ( ( Length(oims) = lens ) and ( Length(rims) = lens ) ) then 
        Error( "oims and/or rims have incorrect length" ); 
    fi; 
    if not ForAll( oims, o -> o in obr ) then 
        Error( "object images not all objects in rng" ); 
    fi; 
    if not ( rims[1] = One( gpr ) ) then 
        Error( "first ray image not the identity" ); 
    fi; 
    rayr := RayElementsOfGroupoid( rng ); 
    pos1 := Position( obr, oims[1] ); 
    for i in [2..lenr] do 
        posi := Position( obr, oims[i] ); 
        if not ( ( rayr[pos1] * rims[i] * rayr[posi]^-1 ) in gpr ) then 
            Error( "ray images not all in relevant homset" ); 
        fi; 
    od;
    return GroupoidHomomorphismFromSinglePieceNC( src, rng, hom, oims, rims ); 
end );

#############################################################################
##
#M  GroupoidAutomorphismByObjectPermNC  
#M  GroupoidAutomorphismByObjectPerm 
##
InstallMethod( GroupoidAutomorphismByObjectPermNC , 
    "for a single piece groupoid and a permutation of objects", true, 
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ], 0,
function( gpd, oims )
    local  rgp, obs, nobs, hom, rims, pos, rays, pos1, ior1, posi, mor, L; 
    rgp := gpd!.magma; 
    hom := InclusionMappingGroups( rgp, gpd!.magma ); 
    obs := gpd!.objects; 
    nobs := Length( obs ); 
    if ( HasIsDirectProductWithCompleteGraph( gpd ) 
         and IsDirectProductWithCompleteGraph( gpd ) ) then 
        rims := ListWithIdenticalEntries( nobs, One( rgp ) ); 
    else 
        rays := gpd!.rays; 
        pos1 := Position( obs, oims[1] ); 
        ior1 := rays[pos1]^-1; 
        rims := List( [1..nobs], i -> ior1 * rays[ Position(obs,oims[i]) ] ); 
    fi; 
    #? (06/07/11) removed NC in next line so that checks are made
    mor := GroupoidHomomorphismFromSinglePiece( gpd, gpd, hom, oims, rims ); 
    SetIsInjectiveOnObjects( mor, true ); 
    SetIsSurjectiveOnObjects( mor, true ); 
    L := [1..nobs]; 
    SortParallel( ShallowCopy( oims ), L );  
    SetOrder( mor, Order( PermList( L ) ) );
    return mor; 
end ); 

InstallMethod( GroupoidAutomorphismByObjectPermNC,  
    "for a hom discrete groupoid and a permutation of objects", true, 
    [ IsHomogeneousDiscreteGroupoid, IsHomogeneousList ], 0,
function( gpd, oims )

    local  gpd1, gp, id, homs, mor, L; 

    gpd1 := Pieces( gpd )[1]; 
    gp := gpd1!.magma; 
    id := IdentityMapping( gp ); 
    homs := List( oims, o -> id ); 
    mor := GroupoidHomomorphismFromHomogeneousDiscreteNC
               ( gpd, gpd, homs, oims ); 
    SetIsInjectiveOnObjects( mor, true ); 
    SetIsSurjectiveOnObjects( mor, true ); 
    L := [1..Length(oims)]; 
    SortParallel( ShallowCopy( oims ), L );  
    SetOrder( mor, Order( PermList( L ) ) );
    return mor; 
end ); 

InstallMethod( GroupoidAutomorphismByObjectPerm, 
    "for a hom discrete groupoid and a permutation of objects", true, 
    [ IsGroupoid, IsHomogeneousList ], 0,
function( gpd, oims )
    local  obs, pos; 
    obs := ObjectList( gpd ); 
    pos := PermList( List( oims, o -> Position( obs, o ) ) ); 
    if ( pos = fail ) then 
        Error( "object images not a permutation of the objects" ); 
    fi; 
    return GroupoidAutomorphismByObjectPermNC( gpd, oims ); 
end ); 

#############################################################################
##
#M  GroupoidAutomorphismByGroupAutoNC  
#M  GroupoidAutomorphismByGroupAuto 
##
InstallMethod( GroupoidAutomorphismByGroupAutoNC , 
    "for a groupoid and an automorphism of the root group", true, 
    [ IsGroupoid and IsSinglePiece, IsGroupHomomorphism and IsBijective ], 0,
function( gpd, hom )
    local  rgp, obs, pos, rays, mor; 
    rgp := gpd!.magma; 
    obs := gpd!.objects;
    rays := RayElementsOfGroupoid( gpd ); 
    mor := GroupoidHomomorphismFromSinglePieceNC( gpd, gpd, hom, obs, rays ); 
    SetIsInjectiveOnObjects( mor, true ); 
    SetIsSurjectiveOnObjects( mor, true ); 
    SetOrder( mor, Order( hom ) ); 
    return mor; 
end ); 

InstallMethod( GroupoidAutomorphismByGroupAuto, 
    "for a groupoid and an automorphism of the root group", true, 
    [ IsGroupoid and IsSinglePiece, IsGroupHomomorphism ], 0,
function( gpd, hom )  
    local  rgp; 
    rgp := gpd!.magma; 
    if not ( (Source(hom) = rgp) and (Range(hom) = rgp) ) then 
        Error( "hom not an endomorphism of the root group" ); 
    fi; 
    if not IsBijective( hom ) then 
        Error( "hom is not an automorphism" ); 
    fi;
    return GroupoidAutomorphismByGroupAutoNC( gpd, hom ); 
end ); 

#############################################################################
##
#M  GroupoidAutomorphismByGroupAutosNC  
#M  GroupoidAutomorphismByGroupAutos 
##
InstallMethod( GroupoidAutomorphismByGroupAutosNC , 
    "for homogeneous discrete groupoid and automorphism list", true, 
    [ IsHomogeneousDiscreteGroupoid, IsHomogeneousList ], 0,
function( gpd, homs )
    local  obs, orders, m; 
    obs := ObjectList( gpd );
    m := GroupoidHomomorphismFromHomogeneousDiscreteNC( gpd, gpd, homs, obs ); 
    SetIsEndoGeneralMapping( m, true ); 
    SetIsInjectiveOnObjects( m, true ); 
    SetIsSurjectiveOnObjects( m, true ); 
    orders := List( homs, h -> Order( h ) ); 
    SetOrder( m, Lcm( orders ) ); 
    return m; 
end ); 

InstallMethod( GroupoidAutomorphismByGroupAutos, 
    "for homogeneous discrete groupoid and automorphism list", true, 
    [ IsHomogeneousDiscreteGroupoid, IsHomogeneousList ], 0,
function( gpd, homs )  

    local  pieces, len, i, p, g, h; 

    pieces := Pieces( gpd ); 
    len := Length( pieces ); 
    if not ( len = Length( homs ) ) then 
        Error( "length of <homs> not equal to number of objects on <gpd>," ); 
    fi; 
    for i in [1..len] do 
        p := pieces[i];  
        g := p!.magma; 
        h := homs[i]; 
        if not ( (Source(h) = g) and (Range(h) = g) ) then 
            Error( "<h> not an endomorphism of group <g> at object <i>," ); 
        fi; 
        if not IsBijective( h ) then 
            Error( "<h> not an automorphism of group <g> at object <i>," ); 
        fi; 
    od; 
    return GroupoidAutomorphismByGroupAutosNC( gpd, homs ); 
end ); 

#############################################################################
##
#M  GroupoidAutomorphismByRayImagesNC  
#M  GroupoidAutomorphismByRayImages 
##
InstallMethod( GroupoidAutomorphismByRayImagesNC , 
    "for a groupoid and a list of ray images", true, 
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ], 0,
function( gpd, images )
    local  obs, hom, mor; 
    obs := gpd!.objects;
    hom := IdentityMapping( gpd!.magma ); 
    mor := GroupoidHomomorphismFromSinglePieceNC( gpd, gpd, hom, obs, images ); 
    SetOrder( mor, Lcm( List( images, i -> Order(i) ) ) ); 
    SetIsInjectiveOnObjects( mor, true ); 
    SetIsSurjectiveOnObjects( mor, true ); 
    return mor;
end ); 

InstallMethod( GroupoidAutomorphismByRayImages, 
    "for a groupoid and a list of images for the rays", true, 
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ], 0,
function( gpd, images )  
    local  rgp, rays, nobs, conj; 
    rgp := gpd!.magma; 
    rays := gpd!.rays; 
    nobs := Length( gpd!.objects );
    if not ( images[1] = One(rgp) ) then 
        Error( "first image must be the identity element" ); 
    fi;
    if not ForAll( [2..nobs], i -> images[i]^(rays[i]^-1) in rgp ) then  
        Error( "ray images not all in the relevant homsets" ); 
    fi; 
    return GroupoidAutomorphismByRayImagesNC( gpd, images ); 
end ); 

#############################################################################
##
#M  AutomorphismGroup( <gpd> )
#M  NiceObjectAutoGroupGroupoid( <gpd> ) . . . . . . create a nice monomorphism 
##
InstallMethod( NiceObjectAutoGroupGroupoid, "for a single piece groupoid", 
    true, [ IsGroupoid and IsSinglePieceDomain ], 0,

function( gpd ) 

    local  rgp, genrgp, nrgp, agp, genagp, nagp, iso1, im1, iso2, pagp, iso, 
           obs, n, L, symm, gensymm, nsymm, ngp, genngp, nngp, ninfo, nemb, 
           gens1, i, pi, imi, j, gens2, k, krgp, ikrgp, 
           pasy, esymm, epagp, genpasy, gens12, actgp, ok, action, sdp, 
           sinfo, siso, ssdp, epasy, engp, gennorm, norm, a, c, c1, c2, c12, 
           agens1, agens2, agens3, agens, nat; 

    ## first: automorphisms of the root group 
    rgp := gpd!.magma; 
    genrgp := GeneratorsOfGroup( rgp );
    nrgp := Length( genrgp ); 
    agp := AutomorphismGroup( rgp ); 
    genagp := GeneratorsOfGroup( agp ); 
    nagp := Length( genagp ); 
    iso1 := IsomorphismPermGroup( agp ); 
    im1 := Image( iso1 );
    iso2 := SmallerDegreePermutationRepresentation( im1 );
    pagp := Image( iso2 ); 
    iso := iso1*iso2; 
    agens1 := List( genagp, a -> Image( iso, a ) ); 
    ## second: permutations of the objects 
    obs := gpd!.objects; 
    n := Length( obs );
    if ( n = 1 ) then 
        return pagp; 
    elif ( n = 2 ) then 
        gensymm := [ (1,2) ]; 
    else 
        L := [2..n]; 
        Append( L, [1] ); 
        gensymm := [ PermList(L), (1,2) ];  #? replace (1,2) with (n-1,n) ?? 
    fi;
    symm := Group( gensymm ); 
    nsymm := Length( gensymm ); 
    ## third: ray products 
    ngp := DirectProduct( ListWithIdenticalEntries( n, rgp ) ); 
    genngp := GeneratorsOfGroup( ngp ); 
    nngp := Length( genngp );
    ninfo := DirectProductInfo( ngp ); 
    for i in [1..n] do 
        k := Embedding( ngp, i ); 
    od; 
    nemb := ninfo!.embeddings; 
    # action of agp on ngp 
    gens1 := [1..nagp]; 
    for i in [1..nagp] do 
        k := genagp[i]; 
        krgp := List( genrgp, r -> Image( k, r ) ); 
        ikrgp := [ ]; 
        for j in [1..n] do 
            Append( ikrgp, List( krgp, x -> Image( nemb[j], x ) ) ); 
        od; 
        gens1[i] := GroupHomomorphismByImages( ngp, ngp, genngp, ikrgp ); 
    od; 
    ## action of symm on ngp = rgp^n 
    gens2 := [1..nsymm]; 
    for i in [1..nsymm] do 
        pi := gensymm[i]^-1; 
        gens2[i] := GroupHomomorphismByImages( ngp, ngp, genngp,  
            List( [1..nngp], j -> genngp[ RemInt( j-1, nrgp ) + 1  
            + nrgp * (( QuoInt( j-1, nrgp ) + 1 )^pi - 1 ) ] ) ); 
    od; 
    pasy := DirectProduct( pagp, symm ); 
    epagp := Embedding( pasy, 1 ); 
    agens1 := List( agens1, g -> Image( epagp, g ) ); 
    esymm := Embedding( pasy, 2 ); 
    agens2 := List( gensymm, g -> Image( esymm, g ) ); 
    genpasy := GeneratorsOfGroup( pasy ); 
    #  construct the semidirect product 
    gens12 := Concatenation( gens1, gens2 ); 
    actgp := Group( gens12 ); 
    ok := IsGroupOfAutomorphisms( actgp ); 
    action := GroupHomomorphismByImages( pasy, actgp, genpasy, gens12 ); 
    sdp := SemidirectProduct( pasy, action, ngp ); 
    Info( InfoGpd, 2, "sdp has ", Length(GeneratorsOfGroup(sdp)), " gens." ); 
    sinfo := SemidirectProductInfo( sdp ); 
    siso := SmallerDegreePermutationRepresentation( sdp ); 
    ssdp := Image( siso ); 
    epasy := Embedding( sdp, 1 ) * siso; 
    agens1 := List( agens1, g -> Image( epasy, g ) );  
    agens2 := List( agens2, g -> Image( epasy, g ) ); 
    engp := Embedding( sdp, 2 ) * siso; 
    agens3 := List( genngp{[3..nngp]}, g -> Image( engp, g ) ); 
    #  construct the normal subgroup 
    gennorm := [1..nrgp ]; 
    for i in [1..nrgp] do 
        c := genrgp[i]; 
        a := GroupHomomorphismByImages( rgp, rgp, genrgp, List(genrgp,x->x^c) );
        c1 := Image( epasy, Image( epagp, Image( iso, a ) ) );
        c := c^-1; 
        c2 := Image( engp, Product( List([1..n], j->Image(nemb[j],c) ) ) ); 
        gennorm[i] := c1 * c2; 
    od; 
    norm := Subgroup( ssdp, gennorm ); 
    ok := IsNormal( ssdp, norm ); 
    if not ok then 
        Error( "norm should be a normal subgroup of ssdp" ); 
    fi; 
    nat := NaturalHomomorphismByNormalSubgroup( ssdp, norm ); 
    agens := List( Concatenation( agens1, agens2, agens3 ), 
                   g -> Image( nat, g ) ); 
    return [ Image( nat ), agens ]; 
end );

InstallMethod( NiceObjectAutoGroupGroupoid, "for a hom discrete groupoid", 
    true, [ IsHomogeneousDiscreteGroupoid ], 0,

function( gpd ) 

    local  pieces, obs, m, p1, g1, geng1, ng1, ag1, genag1, nag1, 
           iso1, ag2, iso2, ag3, nag3, iso, genag3, mag3, genmag3, nmag3, 
           minfo, i, k, memb, L, symm, gensymm, nsymm, imact, pi, 
           actgp, ok, action, sdp, sinfo, siso, ssdp, esymm, egensymm, 
           emag3, egenmag3, agens;  

    pieces := Pieces( gpd ); 
    obs := ObjectList( gpd ); 
    m := Length( obs );
    ## first: automorphisms of the object groups 
    p1 := pieces[1]; 
    g1 := p1!.magma; 
    geng1 := GeneratorsOfGroup( g1 );
    ng1 := Length( geng1 ); 
    ag1 := AutomorphismGroup( g1 ); 
    genag1 := GeneratorsOfGroup( ag1 ); 
    nag1 := Length( genag1 ); 
    iso1 := IsomorphismPermGroup( ag1 ); 
    ag2 := Image( iso1 );
    iso2 := SmallerDegreePermutationRepresentation( ag2 );
    ag3 := Image( iso2 ); 
    iso := iso1*iso2; 
    genag3 := List( genag1, a -> Image( iso, a ) ); 
    nag3 := Length( genag3 ); 
    ## now form the m-fold direct product of ag3
    mag3 := DirectProduct( ListWithIdenticalEntries( m, ag3 ) ); 
    genmag3 := GeneratorsOfGroup( mag3 ); 
    nmag3 := Length( genmag3 );
    minfo := DirectProductInfo( mag3 ); 
    for i in [1..m] do 
        k := Embedding( mag3, i ); 
    od; 
    memb := minfo!.embeddings; 
    ## second: permutations of the objects 
    if ( m = 2 ) then 
        gensymm := [ (1,2) ]; 
    else 
        L := [2..m]; 
        Append( L, [1] ); 
        gensymm := [ PermList(L), (1,2) ];  #? replace (1,2) with (m-1,m) ?? 
    fi;
    symm := Group( gensymm ); 
    nsymm := Length( gensymm ); 
    ## action of symm on mag3 = ag3^m 
    imact := [1..nsymm]; 
    for i in [1..nsymm] do 
        pi := gensymm[i]^-1; 
        imact[i] := GroupHomomorphismByImages( mag3, mag3, genmag3,  
            List( [1..nmag3], j -> genmag3[ RemInt( j-1, nag3 ) + 1  
            + nag3 * (( QuoInt( j-1, nag3 ) + 1 )^pi - 1 ) ] ) ); 
    od; 
    #  construct the semidirect product: symm acting on nag3
    actgp := Group( imact ); 
    ok := IsGroupOfAutomorphisms( actgp ); 
    action := GroupHomomorphismByImages( symm, actgp, gensymm, imact ); 
    sdp := SemidirectProduct( symm, action, mag3 ); 
    Info( InfoGpd, 2, "sdp has ", Length(GeneratorsOfGroup(sdp)), " gens." ); 
    sinfo := SemidirectProductInfo( sdp ); 
    siso := SmallerDegreePermutationRepresentation( sdp ); 
    ssdp := Image( siso ); 
    esymm := Embedding( sdp, 1 ) * siso; 
    egensymm := List( gensymm, g -> Image( esymm, g ) );  
    emag3 := Embedding( sdp, 2 ) * siso; 
    egenmag3 := List( genmag3{[1..nag3]}, g -> Image( emag3, g ) ); 
    agens := Concatenation( egensymm, egenmag3 ); 
    return [ ssdp, agens ]; 
end );

InstallMethod( AutomorphismGroup, "for a single piece groupoid", 
    true, [ IsGroupoid and IsSinglePieceDomain ], 0,

function( gpd ) 
    local  autgen, nautgen, rgp, genrgp, agp, genagp, a, obs, n, imobs, 
           L, ok, id, ids, cids, i, c, aut, niceob, nicemap;  

    Info( InfoGpd, 2, "AutomorphismGroup for single piece groupoids" ); 
    ##  first: automorphisms of the root group 
    autgen := [ ]; 
    rgp := gpd!.magma; 
    genrgp := GeneratorsOfGroup( rgp ); 
    agp := AutomorphismGroup( rgp ); 
    genagp := GeneratorsOfGroup( agp ); 
    for a in genagp do  
        Add( autgen, GroupoidAutomorphismByGroupAuto( gpd, a ) );  
    od; 
    ##  second: permutations of the objects 
    obs := gpd!.objects; 
    n := Length( obs );
    if ( n = 1 ) then 
        Print( "#I  single object case not yet dealt with\n" ); 
        # return pagp; 
    fi; 
    if ( n = 2 ) then 
        imobs := [ obs[2], obs[1] ]; 
        Add( autgen, GroupoidAutomorphismByObjectPerm( gpd, imobs ) );
    else 
        L := [2..n]; 
        Append( L, [1] ); 
        imobs := List( L, i -> obs[i] ); 
        Add( autgen, GroupoidAutomorphismByObjectPerm( gpd, imobs ) );
        imobs := ShallowCopy( obs ); 
        imobs[1] := obs[2]; 
        imobs[2] := obs[1]; 
        Add( autgen, GroupoidAutomorphismByObjectPerm( gpd, imobs ) );
    fi; 
    ##  third: add in the ray prods 
    ids := List( obs, o -> One( rgp ) ); 
    for i in [2..n] do 
        for c in genrgp do 
            cids := ShallowCopy( ids ); 
            cids[i] := c; 
            Add( autgen, GroupoidAutomorphismByRayImages( gpd, cids ) ); 
        od; 
    od; 
    nautgen := Length( autgen ); 
    ##  generating set for the automorphism group now complete 
    for a in autgen do 
        ok := IsAutomorphismWithObjects( a ); 
    od; 
    id := IdentityMapping( gpd ); 
    ## imobs := L[0]; 
    aut := GroupWithGenerators( autgen, id ); 
    SetIsAutomorphismGroup( aut, true ); 
    SetIsFinite( aut, true ); 
    niceob := NiceObjectAutoGroupGroupoid( gpd ); 
    nicemap := GroupHomomorphismByImagesNC( aut, niceob[1], autgen, niceob[2] ); 
    SetNiceMonomorphism( aut, nicemap ); 
    SetIsHandledByNiceMonomorphism( aut, true ); 
    #?  SetInnerAutomorphismsAutomorphismGroup( aut, ?? );  
    return aut; 
end ); 

InstallMethod( AutomorphismGroup, "for a homogeneous discrete groupoid", 
    true, [ IsHomogeneousDiscreteGroupoid ], 0,
function( gpd ) 
    local  pieces, autgen, nautgen, p1, g1, ag1, id1, genag1, a, auts, 
           obs, n, imobs, L, ok, id, ids, aut, niceob, nicemap;  

    Info( InfoGpd, 2, "AutomorphismGroup for homogeneous discrete groupoids" ); 
    pieces := Pieces( gpd ); 
    obs := ObjectList( gpd ); 
    n := Length( obs );
    ##  first: automorphisms of the first object group 
    autgen := [ ]; 
    p1 := pieces[1]; 
    g1 := p1!.magma; 
    ag1 := AutomorphismGroup( g1 ); 
    id1 := IdentityMapping( g1 ); 
    auts := ListWithIdenticalEntries( n, id1 ); 
    genag1 := GeneratorsOfGroup( ag1 ); 
    for a in genag1 do 
        auts[1] := a; 
        Add( autgen, GroupoidAutomorphismByGroupAutos( gpd, auts ) );  
    od; 
    ##  second: permutations of the objects 
    if ( n = 2 ) then 
        imobs := [ obs[2], obs[1] ]; 
        Add( autgen, GroupoidAutomorphismByObjectPerm( gpd, imobs ) );
    else 
        L := [2..n]; 
        Append( L, [1] ); 
        imobs := List( L, i -> obs[i] ); 
        Add( autgen, GroupoidAutomorphismByObjectPerm( gpd, imobs ) );
        imobs := ShallowCopy( obs ); 
        imobs[1] := obs[2]; 
        imobs[2] := obs[1]; 
        Add( autgen, GroupoidAutomorphismByObjectPerm( gpd, imobs ) );
    fi; 
    nautgen := Length( autgen ); 
    ##  generating set for the automorphism group now complete 
    for a in autgen do 
        ok := IsAutomorphismWithObjects( a ); 
    od; 
    id := IdentityMapping( gpd ); 
    ## imobs := L[0];
    aut := GroupWithGenerators( autgen, id ); 
    SetIsAutomorphismGroup( aut, true ); 
    SetIsFinite( aut, true ); 
    niceob := NiceObjectAutoGroupGroupoid( gpd ); 
    nicemap := GroupHomomorphismByImagesNC( aut, niceob[1], autgen, niceob[2] ); 
    SetNiceMonomorphism( aut, nicemap ); 
    SetNiceObject( aut, niceob[1] ); 
    SetIsHandledByNiceMonomorphism( aut, true ); 
    #?  SetInnerAutomorphismsAutomorphismGroup( aut, ?? );  
    return aut; 
end ); 

#############################################################################
##
#M  \=( <m1>, <m2> )  . . . . . . . . . . . equality of two groupoid mappings
##
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
             and ( ImagesOfRays( m1 ) = ImagesOfRays( m1 ) ) ); 
    else 
        return fail; 
    fi; 
end );

#############################################################################
##
#M  \*( <m1>, <m2> )  . . . . . . . . . . . product of two groupoid mappings
##
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
    im12 := List( mgi1[2], x -> Image( h2, x ) ); 
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
#M  ImageElm( <map>, <e> )
##
InstallOtherMethod( ImageElm, "for a groupoid mapping between single pieces", 
    true, [ IsGroupoidHomomorphism and IsHomomorphismFromSinglePiece, 
    IsGroupoidElement ], 0,
function ( map, e )

    local  m1, imo, obs1, pt1, ph1, ray1, rims, loop, g2, iloop;

    Info( InfoGpd, 5, "using ImageElm for IsHomomorphismFromSinglePiece" ); 
    #?  need to include some tests here ?? 
    m1 := Source( map ); 
    imo := ImagesOfObjects( map ); 
    obs1 := m1!.objects; 
    pt1 := Position( obs1, e![2] ); 
    ph1 := Position( obs1, e![3] ); 
    ray1 := RayElementsOfGroupoid( m1 ); 
    loop := ray1[pt1] * e![1] * ray1[ph1]^-1; 
    rims := ImagesOfRays( map ); 
    iloop := ImageElm( RootGroupHomomorphism( map ), loop ); 
    g2 := rims[pt1]^-1 * iloop * rims[ph1]; 
    return ArrowNC( true, g2, imo[pt1], imo[ph1] );
end ); 

InstallOtherMethod( ImageElm, "for a map from homogeneous, discrete groupoid", 
    true, [ IsGeneralMappingFromHomogeneousDiscrete 
            and IsGroupoidHomomorphism, IsGroupoidElement ], 6,
function ( map, e )

    local  p1, t2, g2;

    Info( InfoGpd, 5, 
        "using ImageElm for IsGeneralMappingFromHomogeneousDiscrete" ); 
    #?  need to include some tests here ?? 
    p1 := Position( Source( map )!.objects, e![2] ); 
    t2 := ImagesOfObjects( map )[ p1 ]; 
    g2 := ImageElm( ObjectHomomorphisms( map )[ p1 ], e![1] ); 
    return ArrowNC( true, g2, t2, t2 );
end ); 

InstallOtherMethod( ImageElm, "for a groupoid mapping", true, 
    [ IsGroupoidHomomorphism, IsGroupoidElement ], 0,
function ( map, e )

    local  G1, C1, pe, obs1, pim, obs2, t2, h2, g2;

    Info( InfoGpd, 5, 
        "using ImageElm for general groupoid homomorphisms" ); 
    #?  need to include some tests here ??
    G1 := Source( map ); 
    C1 := Pieces( G1 );
    pe := Position( C1, PieceOfObject( G1, e![2] ) ); 
    obs1 := C1[pe]!.objects; 
    pim := PieceImages( map )[pe]; 
    obs2 := pim[1]; 
    t2 := obs2[ Position( obs1, e![2] ) ];
    h2 := obs2[ Position( obs1, e![3] ) ]; 
    g2 := Image( pim[2], e![1] );
    return Arrow( Range( map ), g2, t2, h2 );
end );

InstallOtherMethod( ImagesRepresentative, "for a groupoid homomorphism", true, 
    [ IsGroupoidHomomorphism, IsGroupoidElement ], 0, 
function( map, e ) 
    return ImageElm( map, e ); 
end );

#############################################################################
##
#M  TestAllProductsUnderGroupoidHomomorphism( <hom> )
##
##  added 12/01/11 to check all z=x*y in costar(o)*star(o) -> iz=ix*iy 
##
InstallMethod( TestAllProductsUnderGroupoidHomomorphism,
    "generic method for a groupoid homomorphism", true,
    [ IsGroupoidHomomorphism ], 0,
function( hom )

    local  t, src, obs, o, st, cst, x, ix, y, iy, z, iz; 
    t := 0; 
    src := Source( hom ); 
    obs := src!.objects; 
    for o in obs do 
        st := ObjectStar( src, o ); 
        cst := ObjectCostar( src, o ); 
        for x in cst do 
            ix := ImageElm( hom, x ); 
            for y in st do 
                iy := ImageElm( hom, y );
                z := x*y; 
                t := t+1; 
                iz := ImageElm( hom, z ); 
                if not ( iz = ix*iy ) then 
                    Print( " x, y, z = ", [x,y,z], 
                           "ix,iy,iz = ", [ix,iy,iz], "\n" ); 
                    return false; 
                fi;
            od;
        od;
    od;
    Print( "t = ", t, " products tested\n" ); 
    return true; 
end ); 

#############################################################################
##
#M  IsomorphismPermGroupoid
##
InstallMethod( IsomorphismPermGroupoid, "for a connected groupoid", true,
    [ IsGroupoid and IsSinglePiece ], 0,
function( g1 )

    local  iso, obs, g, g2p, p, g2;

    obs := g1!.objects;
    g := g1!.magma;
    g2p := IsomorphismPermGroupoid( g );
    p := Image( g2p );
    g2 := SinglePieceMagmaWithObjects( p, obs ); 
    #? (28/01/11) added "NC" and the rays, but not checked! 
    iso := GroupoidHomomorphismFromSinglePieceNC( 
               g1, g2, obs, g2p, RayElementsOfGroupoid( g2 ) );
    SetIsInjectiveOnObjects( iso, true );
    SetIsSurjectiveOnObjects( iso, true );
    return iso;
end );

InstallMethod( IsomorphismPermGroupoid, "generic method for a groupoid", 
    true, [ IsGroupoid ], 0,
function( g1 )

    local  isos, comp1, nc1, i, g2, iso;

    comp1 := Pieces( g1 );
    nc1 := Length( comp1 );
    isos := ListWithIdenticalEntries( nc1, 0 );
    for i in [1..nc1] do 
        isos[i] := IsomorphismPermGroupoid( comp1[i] );
    od;
    g2 := UnionOfPieces( List( isos, m -> Image(m) ) );
    iso := HomomorphismByUnion( g1, g2, isos );
    SetIsInjectiveOnObjects( iso, true );
    SetIsSurjectiveOnObjects( iso, true );
    return iso;
end );

#############################################################################
##
#M  IsomorphismNewObjects
##
InstallMethod( IsomorphismNewObjects, "for a single piece groupoid", true,
    [ IsGroupoid and IsSinglePiece, IsHomogeneousList ], 0,
function( gpd1, ob2 )

    local  iso, ob1, gp, gpd2, id;

    ob1 := gpd1!.objects; 
    if not ( Length(ob1) = Length(ob2) ) then
        Error( "object sets have different lengths" );
    fi;
    gp := gpd1!.magma;
    gpd2 := SinglePieceGroupoidNC( gp, ShallowCopy( Set( ob2 ) ) );
    id := IdentityMapping( gp );
    iso := HomomorphismToSinglePiece( gpd1, gpd2, 
               [ [id, ob2, RayElementsOfGroupoid( gpd1 ) ] ] ); 
    return iso;
end );


## ======================================================================== ##
##                     Homogeneous groupoid homomorphisms                   ##
## ======================================================================== ##

#############################################################################
##
#M  GroupoidHomomorphismFromHomogeneousDiscreteNC 
#M  GroupoidHomomorphismFromHomogeneousDiscrete 
##
InstallMethod( GroupoidHomomorphismFromHomogeneousDiscreteNC,
    "method for a mapping from a homogeneous, discrete groupoid", true,
    [ IsHomogeneousDiscreteGroupoid, IsGroupoid, IsHomogeneousList, 
      IsHomogeneousList ], 0,
function( src, rng, homs, oims )

    local  fam, filter, map, ok, p;

    fam := GroupoidHomomorphismFamily; 
    filter := IsGroupoidHomomorphismFromHomogeneousDiscreteRep 
              and IsGeneralMappingFromHomogeneousDiscrete; 
    map := rec(); 
    ObjectifyWithAttributes( map, NewType( fam, filter ), 
        Source, src, 
        Range, rng, 
        ObjectHomomorphisms, homs, 
        ImagesOfObjects, oims, 
        IsGeneralMappingWithObjects, true, 
        IsGroupoidHomomorphism, true, 
        RespectsMultiplication, true ); 
#    ok := IsInjectiveOnObjects( map ); 
#    p := ObjectTransformationOfGroupoidHomomorphism( map ); 
#    ok := IsSurjectiveOnObjects( map ); 
    SetIsGeneralMappingFromSinglePiece( map, false ); 
    SetPieceImages( map, [ [ homs, oims, [ ] ] ] );
#    if ( src = rng ) then 
#        SetIsEndoGeneralMapping( map, true ); 
#        SetIsEndomorphismWithObjects( map, true ); 
        ## ok := IsAutomorphismWithObjects( map ); 
#    fi; 
    return map; 
end );

InstallMethod( GroupoidHomomorphismFromHomogeneousDiscrete,
    "method for a mapping from a homogeneous, discrete groupoid", true,
    [ IsHomogeneousDiscreteGroupoid, IsGroupoid and IsDiscreteDomainWithObjects, 
      IsHomogeneousList, IsHomogeneousList ], 0,
function( src, rng, homs, oims ) 

    local  gps, gpr, obs, obr; 

    gps := src!.magma; 
    gpr := rng!.magma; 
    if not ForAll( homs, 
        h -> ( Source(h) = gps ) and ( Range(h) = gpr ) ) then 
        Error( "homs not a list of maps RootGroup(src) -> RootGroup(rng) " ); 
    fi; 
    obs := src!.objects; 
    obr := rng!.objects; 
    if not ( Length(oims) = Length(obs) ) then 
        Error( "<oims> has incorrect length" ); 
    fi; 
    if not ForAll( oims, o -> o in obr ) then 
        Error( "object images not all objects in <rng>" ); 
    fi; 
    return GroupoidHomomorphismFromHomogeneousDiscreteNC( src,rng,homs,oims ); 
end );

InstallMethod( GroupoidHomomorphismFromHomogeneousDiscrete,
    "method for a mapping from a homogeneous, discrete groupoid", true,
    [ IsHomogeneousDiscreteGroupoid, IsGroupoid, IsHomogeneousList, 
      IsHomogeneousList ], 0,
function( src, rng, homs, oims ) 

    local  gps, obs, obr, lens; 

    obs := src!.objects; 
    obr := rng!.objects; 
    lens := Length( obs ); 
    if not ( Length(oims) = lens ) then 
        Error( "<oims> has incorrect length" ); 
    fi; 
    if not ForAll( oims, o -> o in obr ) then 
        Error( "object images not all objects in <rng>" ); 
    fi; 
    gps := src!.magma; 
    if not ForAll( [1..lens], 
        j -> ( Source( homs[j] ) = gps ) 
               and ( Range( homs[j] ) = ObjectGroup( rng, oims[j] ) ) ) then 
        Error("homs not a list of maps ObjectGroup(src) -> ObjectGroup(rng),"); 
    fi; 
    return GroupoidHomomorphismFromHomogeneousDiscreteNC( src,rng,homs,oims ); 
end );

##############################################################################
##
#E  gpdhom.gi  . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##
