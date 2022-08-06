############################################################################# 
## 
#W  gpd.gi                 GAP4 package `groupoids'             Chris Wensley 
#W                                                               & Emma Moore
#Y  Copyright (C) 2000-2022, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

#############################################################################
##  Standard error messages

GPD_CONSTRUCTORS := Concatenation( 
    "The standard operations which construct a groupoid are:\n",
    "1.  SinglePieceGroupoid( object group, list of objects );\n",
    "2.  DomainWithSingleObject( group, single object );\n",
    "3.  UnionOfPieces( list of groupoids );\n",
    "4.  SinglePieceSubgroupoidByGenerators( list of elements );\n", 
    "5.  SubgroupoidWithRays( parent gpd, root gp, ray mults. );\n", 
    "6.  Groupoid( one of the previous parameter options );" );
##  these are called by the GlobalFunction Groupoid 

SUB_CONSTRUCTORS := Concatenation( 
    "The standard operations which construct a subgroupoid are:\n", 
    "1.  SubgroupoidByPieces( groupoid, list of [imobs,hom] pairs );\n",
    "2.  SubgroupoidBySubgroup( groupoid, group );\n", 
    "3.  SubgroupoidByObjects( groupoid, list of objects );\n", 
    "4.  MaximalDiscreteSubgroupoid( groupoid );\n", 
    "5.  DiscreteSubgroupoid( groupoid, list of obs, list of subgps );\n",
    "6.  FullTrivialSubgroupoid( groupoid );\n", 
    "7.  DiscreteTrivialSubgroupoid( groupoid );\n", 
    "8.  Subgroupoid( one of the previous parameter options );" );
##  and these are called by the GlobalFunction Subgroupoid 

#############################################################################
##
#M  SinglePieceGroupoidNC                                            
#M  SinglePieceGroupoid                                            
##
InstallMethod( SinglePieceGroupoidNC, "method for a connected groupoid",
    true, [ IsGroup, IsHomogeneousList ], 0,
function( gp, obs ) 

    local gpd, gens;

    gpd := rec( objects := obs, magma := gp, 
                rays := List( obs, o -> One( gp ) ) ); 
    ObjectifyWithAttributes( gpd, IsGroupoidType, 
        IsSinglePieceDomain, true,
        IsAssociative, true, 
        IsCommutative, IsCommutative( gp ), 
        IsDirectProductWithCompleteDigraphDomain, true ); 
    gens := GeneratorsOfMagmaWithObjects( gpd ); 
    SetIsDiscreteDomainWithObjects( gpd, Length(obs) = 1 );
    return gpd; 
end );

InstallMethod( SinglePieceGroupoid, "method for a connected groupoid",
    true, [ IsGroup, IsHomogeneousList ], 0,
function( gp, obs ) 
    if not IsSet( obs ) then 
        Sort( obs ); 
    fi; 
    if not IsDuplicateFree( obs ) then
        Error( "objects must be distinct," );
    fi; 
    return SinglePieceGroupoidNC( gp, obs );
end ); 

#############################################################################
##
#M  GroupoidByIsomorphisms
##
InstallMethod( GroupoidByIsomorphisms, 
    "generic method for a group, a set of objects, and a set of isos", true, 
    [ IsGroup, IsHomogeneousList, IsList ], 0,
function( rgp, obs, isos )

    local fam, gpd, gps, i, iso, inv;

    if not ( Length( obs ) = Length( isos ) ) then 
        Error( "obs and isos should have the same length" ); 
    fi; 
    fam := IsGroupoidFamily;
    gpd := rec( objects := obs, magma := rgp, isomorphisms := isos ); 
    ObjectifyWithAttributes( gpd, IsSinglePieceRaysType, 
        IsDirectProductWithCompleteDigraph, false, 
        IsSinglePieceDomain, true, 
        IsGroupoidByIsomorphisms, true ); 
    gps := ShallowCopy( obs ); 
    gps[1] := rgp; 
    for i in [1..Length(obs)] do 
        iso := isos[i]; 
        if not ( IsGroupHomomorphism( iso ) and IsBijective( iso ) ) 
           and not ( IsGroupoidHomomorphism(iso) and IsBijective(iso) ) then 
            Error( "expecting the isos to be group or groupoid isomorphisms" ); 
        fi; 
        gps[i] := Image( iso );
        inv := InverseGeneralMapping( iso ); 
    od; 
    SetObjectGroups( gpd, gps ); 
    gpd!.rays := List( gps, g -> [ One(rgp), One(g) ] ); 
    return gpd; 
end );



#############################################################################
##
#M  SubgroupoidWithRays
#M  SubgroupoidWithRaysNC 
##
InstallMethod( SubgroupoidWithRaysNC, 
    "generic method for a connected gpd with variable object gps", true, 
    [ IsGroupoid, IsGroup, IsHomogeneousList ], 0,
function( pgpd, rgp, rays )

    local obs, rob, fam, filter, gpd, id;

    fam := IsGroupoidFamily;
    ## filter := IsSinglePieceRaysRep; 
    gpd := rec( objects := pgpd!.objects, magma := rgp, rays := rays ); 
    ObjectifyWithAttributes( gpd, IsSinglePieceRaysType, 
        IsSinglePieceDomain, true, 
        LargerDirectProductGroupoid, pgpd ); 
    SetRaysOfGroupoid( gpd, rays );
    SetParent( gpd, pgpd );
    id := One( pgpd!.magma ); 
    SetIsDirectProductWithCompleteDigraphDomain( gpd, 
        ForAll( rays, r -> r = id ) );
    return gpd; 
end );

InstallMethod( SubgroupoidWithRays, 
    "generic method for a connected gpd with variable object gps", true, 
    [ IsGroupoid and IsSinglePiece, IsGroup, IsHomogeneousList ], 0,
function( gpd, rgp, rays )

    local obs, gp, par, grays;

    obs := gpd!.objects; 
    if not ( Length( obs ) = Length( rays ) ) then 
        Error( "should be one ray element for each object in the groupoid," ); 
    fi; 
    if not IsSubgroup( gpd!.magma, rgp ) then
        Error( "subgroupoid root group not a subgroup of the root group," );
    fi;
    grays := RaysOfGroupoid( gpd ); 
    gp := gpd!.magma; 
    if not ( rays[1] = One( rgp ) ) then
        Error( "first ray element is not the identity element," );
    fi;
    if not ForAll( [2..Length(obs)], 
                   i -> rays[i] * grays[i]^-1 in gp ) then 
        Error( "not all the rays are in the corresponding homsets," );
    fi; 
    if HasLargerDirectProductGroupoid( gpd ) then   ## for RaysRep 
        par := LargerDirectProductGroupoid( gpd ); 
    else 
        par := gpd; 
    fi;
    return SubgroupoidWithRaysNC( par, rgp, rays );
end );

#############################################################################
##
#M  SinglePieceSubgroupoidByGenerators
##
InstallMethod( SinglePieceSubgroupoidByGenerators, "for a list of elements",
    true, [ IsGroupoid, IsList ], 0,
function( anc, gens ) 

    local ok, ngens, lpos, loops, ro, go, found, obs, nobs, i, gp, rpos, 
          g, c, p, q, r, rays, par; 

    ok := ForAll( gens, g -> ( FamilyObj(g) = IsGroupoidElementFamily ) ); 
    if not ok then 
        Error( "list supplied is not a list of groupoid elements," ); 
    fi;  
    ngens := Length( gens );
    loops := ListWithIdenticalEntries( ngens, false ); 
    obs := ListWithIdenticalEntries( ngens + ngens, 0 );
    lpos := [ ]; 
    for i in [1..ngens] do 
        obs[i] := gens[i]![2]; 
        obs[ngens+i] := gens[i]![3]; 
        if ( gens[i]![2] = gens[i]![3] ) then 
            loops[i] := true; 
            Add( lpos, i ); 
        fi; 
    od; 
    obs := Set( obs ); 
    ro := obs[1]; 
    nobs := Length( obs ); 
    if ( lpos = [ ] ) then 
        Error( "case with no loops not yet implemented," ); 
    fi; 
    go := gens[ lpos[1] ]![2]; 
    for i in lpos do 
        if ( go <> gens[i]![2] ) then 
            Error( "loop at more than one object not yet implemented," ); 
        fi; 
    od; 
    gp := Group( List( lpos, j -> gens[j]![1] ) ); 
    if not ( ngens - Length( lpos ) - nobs + 1 = 0 ) then 
        Error( "only case (group generators) + (rays) implemented," ); 
    fi; 
    ## find positions of the rays 
    rpos := ListWithIdenticalEntries( nobs, 0 );
    for i in [1..ngens] do 
        if not loops[i] then 
            g := gens[i]; 
            c := g![1]; 
            p := g![2]; 
            q := g![3]; 
            if ( p = go ) then 
                rpos[ Position( obs, q ) ] := i; 
            else 
                Error( "this case not yet implemented," ); 
            fi;
        fi; 
     od; 
    if ( rpos[1] = 0 ) then 
        rpos[1] := lpos[1]; 
        rays := List( rpos, i -> gens[i]![1] ); 
        rays[1] := One( gp ); 
    else 
        ## move the group object to the root object 
        gp := gp^(gens[rpos[1]]![1]); 
        rays := ListWithIdenticalEntries( nobs, 0 ); 
        rays[1] := One( gp ); 
        r := (gens[ rpos[1] ]![1])^-1; 
        for i in [2..nobs] do 
            if ( rpos[i] = 0 ) then 
                rays[i] := r; 
            else 
                rays[i] := r * gens[ rpos[i] ]![1]; 
            fi;  
        od; 
    fi; 
    par := SubgroupoidByObjects( anc, obs ); 
    return SubgroupoidWithRays( par, gp, rays ); 
end );

#############################################################################
##
#M  SinglePieceGroupoidWithRaysNC                                            
#M  SinglePieceGroupoidWithRays                                            
##
InstallMethod( SinglePieceGroupoidWithRaysNC, "method for a connected groupoid",
    true, [ IsGroup, IsHomogeneousList, IsHomogeneousList ], 0,
function( gp, obs, rays ) 

    local gpd, gens1, gens;

    gpd := rec( objects := obs, magma := gp, rays := rays ); 
    ObjectifyWithAttributes( gpd, IsSinglePieceRaysType, 
        IsSinglePieceDomain, true,
        IsAssociative, true, 
        IsCommutative, IsCommutative( gp ), 
        IsSinglePieceGroupoidWithRays, true, 
        IsDirectProductWithCompleteDigraphDomain, false ); 
    gens := GeneratorsOfMagmaWithObjects( gpd ); 
    return gpd; 
end );

InstallMethod( SinglePieceGroupoidWithRays, "method for a connected groupoid",
    true, [ IsGroup, IsHomogeneousList, IsHomogeneousList ], 0,
function( gp, obs, rays ) 
    if not IsSet( obs ) then 
        Sort( obs ); 
    fi; 
    if not IsDuplicateFree( obs ) then
        Error( "objects must be distinct," );
    fi; 
    if not ( Length( obs ) = Length( rays ) ) then 
        Error( "obs and rays should have the same length" ); 
    fi;
    #?  how detailed should tests on the rays be? 
    if ( One( gp ) * rays[1] = fail ) then 
        Error( "cannot compose One(gp) with the first ray" ); 
    fi;
    return SinglePieceGroupoidWithRaysNC( gp, obs, rays );
end ); 

#############################################################################
##
#M  RootGroup
##
InstallMethod( RootGroup, "for a connected groupoid",
    true, [ IsGroupoid and IsSinglePiece ], 0,
function( G )
    return G!.magma; 
end );

#############################################################################
##
#M  RaysOfGroupoid
#M  RayArrowsOfGroupoid
##
InstallMethod( RaysOfGroupoid, "for a connected groupoid",
    true, [ IsGroupoid and IsSinglePiece ], 0,
function( G ) 
    return G!.rays; 
end );

InstallMethod( RaysOfGroupoid, "for a groupoid", true, [ IsGroupoid ], 0,
function( G ) 
    return List( Pieces( G ), p -> RaysOfGroupoid( p ) ); 
end ); 

InstallMethod( RayArrowsOfGroupoid, "for a connected groupoid",
    true, [ IsGroupoid and IsSinglePiece ], 0,
function( gpd ) 
    local obs, root, elts; 
    obs := ObjectList( gpd ); 
    root := obs[1]; 
    elts := RaysOfGroupoid( gpd ); 
    return List( [1..Length(obs)], 
                 i -> ArrowNC( true, elts[i], root, obs[i] ) );
end );

InstallMethod( RayArrowsOfGroupoid, "for a groupoid", true, [ IsGroupoid ], 0,
function( G ) 
    return List( Pieces( G ), p -> RayArrowsOfGroupoid( p ) ); 
end ); 

#############################################################################
##
#M  Pieces 
##
InstallMethod( Pieces, "for a homogeneous, discrete groupoid",
    true, [ IsHomogeneousDiscreteGroupoid ], 0,
function( gpd ) 
    return List( gpd!.objects, o -> DomainWithSingleObject( gpd!.magma, o ) );
end );

#############################################################################
##
#M  GeneratorsOfGroupoid
##
InstallMethod( GeneratorsOfGroupoid, "for a single piece groupoid", true, 
    [ IsGroupoid and IsSinglePiece ], 0, 
function( gpd )

    local obs, nobs, o1, m, mgens, id, gens1, gens2, gens, rays; 

    obs := gpd!.objects;
    nobs := Length( obs );
    o1 := obs[1];
    m := gpd!.magma; 
    mgens := GeneratorsOfGroup( m ); 
    id := One( m ); 
    if ( HasIsGroupoidByIsomorphisms( gpd ) 
         and IsGroupoidByIsomorphisms( gpd ) ) then 
        gens1 := List( mgens, g -> Arrow( gpd, [ g, g ], o1, o1 ) ); 
    else 
        gens1 := List( mgens, g -> ArrowNC( true, g, o1, o1 ) ); 
    fi;
    if ( HasIsDirectProductWithCompleteDigraph( gpd ) 
        and IsDirectProductWithCompleteDigraph( gpd ) ) then 
        gens2 := List( obs{[2..nobs]}, o -> GroupoidElement(gpd,id,o1,o) ); 
    elif IsSinglePieceRaysRep( gpd ) then 
        rays := gpd!.rays; 
        gens2 := List( [2..nobs], i -> GroupoidElement(gpd,rays[i],o1,obs[i]) ); 
    fi; 
    gens := Immutable( Concatenation( gens1, gens2 ) ); 
    SetGeneratorsOfGroupoid( gpd, gens ); 
    return gens; 
end );

InstallMethod( GeneratorsOfGroupoid, "for a groupoid", true, [ IsGroupoid ], 0,
function( gpd ) 
    return Flat( List( Pieces( gpd ), p -> GeneratorsOfGroupoid( p ) ) ); 
end );

#############################################################################
##
#M  IsPermGroupoid
#M  IsFpGroupoid
#M  IsPcGroupoid
#M  IsMatrixGroupoid
##
InstallMethod( IsPermGroupoid, "for a groupoid", true,
    [ IsGroupoid ], 0,
function( gpd ) 
    if IsSinglePiece( gpd ) then 
        return ( IsPermCollection(gpd!.magma) and IsPermGroup(gpd!.magma) ); 
    else 
        return ForAll( Pieces(gpd), c -> IsPermGroupoid(c) ); 
    fi; 
end );

InstallMethod( IsFpGroupoid, "for a groupoid", true,
    [ IsGroupoid ], 0,
function( gpd )
    if IsSinglePiece( gpd ) then 
        return ( IsGroupOfFamily(gpd!.magma) and IsFpGroup(gpd!.magma) ); 
    else 
        return ForAll( Pieces(gpd), c -> IsFpGroupoid(c) );   
    fi; 
end );

InstallMethod( IsPcGroupoid, "for a groupoid", true,
    [ IsGroupoid ], 0,
function( gpd )
    if IsSinglePiece( gpd ) then 
        return ( HasIsPolycyclicGroup( gpd!.magma )
                 and IsPolycyclicGroup( gpd!.magma ) ); 
    else 
        return ForAll( Pieces(gpd), c -> IsPcGroupoid(c) );   
    fi; 
end ); 

InstallMethod( IsMatrixGroupoid, "for a groupoid", true,
    [ IsGroupoid ], 0,
function( gpd ) 

    local gens; 

    if IsSinglePiece( gpd ) then 
        gens := GeneratorsOfGroup( gpd!.magma ); 
        return ForAll( gens, g ->  HasIsRectangularTable( g )
                                  and IsRectangularTable( g ) ); 
    else 
        return ForAll( Pieces(gpd), c -> IsMatrixGroupoid(c) );   
    fi; 
end ); 

InstallMethod( IsFreeGroupoid, "for a groupoid", true,
    [ IsGroupoid ], 0,
function( gpd )
    if IsSinglePiece( gpd ) then 
        return IsFreeGroup( gpd!.magma ); 
    else 
        return ForAll( Pieces(gpd), c -> IsFreeGroupoid(c) );   
    fi; 
end ); 

#############################################################################
##
#M  DomainWithSingleObject
##
InstallOtherMethod( DomainWithSingleObject, "no object specified", true, 
    [ IsGroup ], 0, 
function( gp ) 
    return DomainWithSingleObject( gp, 0 ); 
end );

InstallMethod( DomainWithSingleObject, "generic method for a group",
    true, [ IsGroup, IsObject ], 0,
function( gp, obj ) 

    local o, gpd; 

    if ( IsList( obj ) and ( Length(obj) = 1 ) ) then
        o := obj[1]; 
    elif IsObject( obj ) then 
        o := obj;
    else 
        Error( "usage: DomainWithSingleObject( <group>, <object> );" ); 
    fi; 
    gpd := SinglePieceGroupoidNC( gp, [ o ] ); 
##    if ( HasIsAutomorphismGroupOfGroupoid( gp ) 
##         and IsAutomorphismGroupOfGroupoid( gp ) ) then 
##        SetIsAutomorphismGroupOfGroupoidAsGroupoid( gpd, true ); 
##    fi;
    return gpd; 
end );

#############################################################################
##
#F  Groupoid( <pieces> )                groupoid as list of pieces 
#F  Groupoid( <gp>, <obj> )             group as groupoid 
#F  Groupoid( <gp>, <obs> )             single piece groupoid 
#F  Groupoid( <gpd>, <rgp>, <rays> )    subgroupoid by root groups, rays 
##
InstallGlobalFunction( Groupoid, function( arg )

    local nargs, id, rays;

    nargs := Length( arg ); 
    # list of pieces
    if ( ( nargs = 1 ) and IsList( arg[1] ) 
         and  ForAll( arg[1], G -> IsGroupoid(G) ) ) then
        Info( InfoGroupoids, 2, "ByUnion" );
        return UnionOfPieces( arg[1] );
    # group * tree groupoid
    elif ( ( nargs = 2 ) and IsList( arg[2] ) and IsGroup( arg[1] ) ) then
        Info( InfoGroupoids, 2, "group plus objects" ); 
        return SinglePieceGroupoid( arg[1], arg[2] );
    # one-object groupoid
    elif ( ( nargs = 2 ) and IsObject( arg[2] ) and IsGroup( arg[1] ) ) then
        Info( InfoGroupoids, 2, "SingleObject" );
        return DomainWithSingleObject( arg[1], arg[2] );
    elif ( ( nargs = 3 ) and IsGroupoid( arg[1] ) and IsGroup( arg[2] ) 
           and IsHomogeneousList( arg[3] ) ) then 
        return SubgroupoidWithRays( arg[1], arg[2], arg[3] );
    else
        Info( InfoGroupoids, 1, GPD_CONSTRUCTORS );
        return fail;
    fi;
end );

#############################################################################
##
#M  String, ViewString, PrintString, ViewObj, PrintObj
##
InstallMethod( String, "for a groupoid", true, [ IsGroupoid ], 0, 
function( gpd ) 
    if IsSinglePiece( gpd ) then 
        return( STRINGIFY( "single piece groupoid with ", 
                           String(Length(ObjectList(gpd))), " objects") ); 
    else 
        return( STRINGIFY( "groupoid with ", 
                           String(Length(Pieces(gpd))), " pieces" ) );
    fi;
end );

InstallMethod( ViewString, "for a groupoid", true, [ IsGroupoid ], 0, String ); 

InstallMethod( PrintString, "for a groupoid", true, [ IsGroupoid ], 0, String ); 

InstallMethod( ViewObj, "for a groupoid", true, [ IsGroupoid ], 0, PrintObj ); 

InstallMethod( PrintObj, "for a groupoid", true, [ IsGroupoid ], 0,
function ( gpd )

    local comp, len, c, i;

    if ( HasIsPermGroupoid( gpd ) and IsPermGroupoid( gpd ) ) then
        Print( "perm " );
    elif ( HasIsFpGroupoid( gpd ) and IsFpGroupoid( gpd ) ) then 
        Print( "fp " );
    elif ( HasIsPcGroupoid( gpd ) and IsPcGroupoid( gpd ) ) then
        Print( "pc " );
    fi; 
    if IsSinglePiece( gpd ) then  
        if IsDirectProductWithCompleteDigraph( gpd ) then 
            Print( "single piece groupoid: < " ); 
            Print( gpd!.magma, ", ", gpd!.objects, " >" );
        else 
            Print( "single piece groupoid with rays: < " ); 
            Print( gpd!.magma, ", ", gpd!.objects, ", ", 
                                     gpd!.rays, " >" );
        fi; 
    elif ( HasIsHomogeneousDiscreteGroupoid( gpd ) 
           and IsHomogeneousDiscreteGroupoid( gpd ) ) then 
            Print( "homogeneous, discrete groupoid: < " ); 
            Print( gpd!.magma, ", ", gpd!.objects, " >" ); 
    else 
        comp := Pieces( gpd ); 
        len := Length( comp ); 
        if ( HasIsHomogeneousDomainWithObjects( gpd ) 
             and IsHomogeneousDomainWithObjects( gpd ) ) then 
            Print( "homogeneous " ); 
        fi; 
        Print( "groupoid with ", len, " pieces:\n" ); 
        if ForAll( comp, c -> HasName(c) ) then 
            Print( comp ); 
        else 
            for i in [1..len-1] do
                c := comp[i]; 
                Print( i, ":  ", c, "\n" );
            od; 
            Print( len, ":  ", comp[len] ); 
        fi;  
    fi;
end );

##############################################################################
##
#M  Display( <gpd> ) . . . . . . . . . . . . . . . . . . .  display a groupoid
##
InstallMethod( Display, "for a groupoid", [ IsGroupoid ],
function( gpd )
    
    local comp, c, i, pgpd, gp, len, rgp, rays;

    if ( HasIsPermGroupoid( gpd ) and IsPermGroupoid( gpd ) ) then
        Print( "perm " );
    elif ( HasIsFpGroupoid( gpd ) and IsFpGroupoid( gpd ) ) then
        Print( "fp " );
    elif ( HasIsPcGroupoid( gpd ) and IsPcGroupoid( gpd ) ) then
        Print( "pc " );
    fi;
    if IsSinglePiece( gpd ) then 
        if IsDirectProductWithCompleteDigraph( gpd ) then 
            Print( "single piece groupoid: " );
            if HasName( gpd ) then
                Print( gpd );
            fi;
            Print( "\n" ); 
            Print( "  objects: ", gpd!.objects, "\n" );
            gp := gpd!.magma;
            Print( "    group: " );
            if HasName( gp ) then
                Print( gp, " = <", GeneratorsOfGroup( gp ), ">\n" );
            else
                Print( gp, "\n" );
            fi; 
        elif ( HasIsGroupoidByIsomorphisms( gpd ) 
               and IsGroupoidByIsomorphisms( gpd ) ) then 
            Print( "single piece groupoid by isomorphisms: " ); 
            if HasName( gpd ) then
                Print( gpd );
            fi;
            Print( "\n" ); 
            Print( "      objects: ", gpd!.objects, "\n" );
            rgp := gpd!.magma;
            Print( "   root group: " );
            if HasName( rgp ) then
                Print( rgp, " = <", GeneratorsOfGroup( rgp ), ">\n" );
            else
                Print( rgp, "\n" );
            fi; 
            Print( " isomorphisms: " ); 
            Perform( gpd!.isomorphisms, Display );
        else
            Print( "single piece groupoid with rays having: " );
            if HasName( gpd ) then
                Print( gpd );
            fi;
            Print( "\n" ); 
            pgpd := LargerDirectProductGroupoid( gpd );
            Print( "supergroupoid: ", pgpd, "\n" ); 
            Print( "      objects: ", gpd!.objects, "\n" );
            rgp := gpd!.magma;
            Print( "   root group: " );
            if HasName( rgp ) then
                Print( rgp, " = <", GeneratorsOfGroup( rgp ), ">\n" );
            else
                Print( rgp, "\n" );
            fi; 
            Print( "         rays: ", gpd!.rays, "\n" );
        fi; 
    elif ( HasIsHomogeneousDiscreteGroupoid( gpd ) 
           and IsHomogeneousDiscreteGroupoid( gpd ) ) then 
        Print( "homogeneous, discrete groupoid with:\n" ); 
        gp := gpd!.magma; 
        Print( "  group: " ); 
        if HasName( gp ) then
            Print( gp, " = <", GeneratorsOfGroup( gp ), "> >\n" );
        else
            Print( gp, " >\n" );
        fi; 
        Print( "objects: ", gpd!.objects, "\n" ); 
    else
        comp := Pieces( gpd );
        len := Length( comp ); 
        if ( HasIsHomogeneousDomainWithObjects( gpd ) 
             and IsHomogeneousDomainWithObjects( gpd ) ) then 
            Print( "homogeneous " ); 
        fi; 
        Print( "groupoid with ", len, " pieces:\n" );
        for i in [1..len] do
            c := comp[i];
            if IsDirectProductWithCompleteDigraph( c ) then 
                Print( "< objects: ", c!.objects, "\n" );
                gp := c!.magma;
                Print( "    group: " );
                if HasName( gp ) then
                    Print( gp, " = <", GeneratorsOfGroup( gp ), "> >\n" );
                else
                    Print( gp, " >\n" );
                fi;
            else
                Print( "<     objects: ", c!.objects, "\n" ); 
                Print( "   parent gpd: ", Parent( c ), "\n" ); 
                rgp := c!.magma; 
                Print( "   root group: " );
                if HasName( rgp ) then
                    Print( rgp, " = <", GeneratorsOfGroup( rgp ), ">\n" );
                else
                    Print( rgp, "\n" );
                fi; 
                Print( "  conjugators: ", c!.rays, "\n" );
            fi; 
        od;
    fi;
end );

#############################################################################
##
#M  \=( <G1>, <G2> )  . . . . . . . . . . . . test if two groupoids are equal
##
InstallMethod( \=, "for a connected groupoid", true, ## IsIdenticalObj,
    [ IsGroupoid and IsSinglePiece, IsGroupoid ], 
function ( G1, G2 )

    ## Print( "### method 1 for =\n" ); 
    if not IsSinglePiece( G2 ) then
        return false;
    fi; 
    if not ( IsDirectProductWithCompleteDigraph( G1 ) = 
             IsDirectProductWithCompleteDigraph( G2 ) ) then
        return false;
    fi;
    if IsDirectProductWithCompleteDigraph( G1 ) then
        return ( ( G1!.objects = G2!.objects ) and ( G1!.magma = G2!.magma ) ); 
    elif ( HasIsGroupoidByIsomorphisms( G1 ) 
           and IsGroupoidByIsomorphisms( G1 ) ) then 
        return ( HasIsGroupoidByIsomorphisms( G2 ) 
                 and IsGroupoidByIsomorphisms( G2 ) 
                 and ( G1!.objects = G2!.objects ) 
                 and ( ObjectGroups( G1 ) = ObjectGroups( G2 ) ) 
                 and ( G1!.isomorphisms = G2!.isomorphisms ) ); 
    elif ( IsSinglePieceRaysRep( G1 ) and IsSinglePieceRaysRep( G2 ) ) then 
        return ( ( Parent( G1 ) = Parent( G2 ) ) and 
                  ( G1!.magma = G2!.magma ) and 
                  ForAll( [1..Length(G1!.rays)], 
                      j -> G1!.rays[j] * G2!.rays[j]^-1 in G1!.magma ) ); 
    else 
        Error( "method not found for G1=G2," ); 
    fi;
end );

InstallMethod( \=, "for a groupoid", true, [ IsGroupoid, IsGroupoid ], 
function ( G1, G2 )
    local c1, c2, len, obj, i, j;

    ## Print( "### method 2 for =\n" ); 
    c1 := Pieces( G1 );
    c2 := Pieces( G2 );
    len := Length( c1 );
    if ( ( len <> Length(c2) ) or ( ObjectList(G1) <> ObjectList(G2) ) ) then
        return false;
    fi;
    for i in [1..len] do
        obj := c1[i]!.objects[1];
        j := PieceNrOfObject( G2, obj );
        if ( c1[i] <> c2[j] ) then
            return false;
        fi;
    od;
    return true;
end );

############################################################################# 
## 
#M  ObjectGroup
## 
InstallMethod( ObjectGroup, "generic method for single piece gpd and object",
    true, [ IsGroupoid and IsSinglePiece, IsObject ], 0,
function( G, obj )

    local obs, nobs, pos, gps, i, c, rgp; 

    obs := G!.objects; 
    if not ( obj in obs ) then
        Error( "obj not an object of G," );
    fi;
    if IsDirectProductWithCompleteDigraph( G ) then 
        return G!.magma;
    fi; 
    pos := Position( obs, obj ); 
    if HasObjectGroups(G) then 
        return ObjectGroups(G)[pos];
    else 
        ## construct all the object groups 
        nobs := Length( obs ); 
        gps := ListWithIdenticalEntries( nobs, 0 ); 
        rgp := G!.magma; 
        gps[1] := rgp; 
        for i in [2..nobs] do
            c := G!.rays[i]; 
            gps[i] := rgp^c; 
        od;
        return gps[pos];
    fi;
end );

InstallMethod( ObjectGroup, "generic method for groupoid and object",
    true, [ IsGroupoid, IsObject ], 0,
function( G, obj )

    local nC, C;

    if not ( obj in ObjectList( G ) ) then
        Error( "obj not an object of G," );
    fi;
    nC := PieceNrOfObject( G, obj ); 
    C := Pieces( G )[ nC ];
    return ObjectGroup( C, obj );
end );

InstallMethod( ObjectGroup, "generic method for single piece gpd with rays",
    true, [ IsSinglePieceGroupoidWithRays, IsObject ], 0,
function( G, obj )

    local H, pieceH, obs, np, p, genp;

    ##  added 11/09/18 to deal with automorphism gpds of homogeneous gpds 
    if HasAutomorphismDomain( G ) then 
        H := AutomorphismDomain( G ); 
        pieceH := Pieces( H ); 
        obs := List( pieceH, p -> ObjectList( p ) ); 
        if not ( obj in obs ) then
            Error( "obj not an object of G," );
        fi;
        np := Position( obs, obj ); 
        p := pieceH[ np ]; 
        if not IsGroupoid( p ) then 
            Error( "P is not a groupoid" ); 
        fi; 
        return AutomorphismGroupOfGroupoid( p ); 
    else 
        TryNextMethod(); 
    fi;
end );

############################################################################# 
## 
#M  ObjectGroups
## 
InstallMethod( ObjectGroups, "generic method for groupoid", true,
    [ IsGroupoid and IsSinglePiece ], 0,
function( gpd )
    return List( gpd!.objects, o -> ObjectGroup( gpd, o ) );
end );

InstallMethod( ObjectGroups, "generic method for groupoid", true,
    [ IsGroupoid ], 0,
function( gpd )
    return List( Pieces( gpd ), C -> ObjectGroups( C ) );
end );


## ======================================================================== ##
##                           Homogeneous groupoids                          ##
## ======================================================================== ##

#############################################################################
##
#M  HomogeneousGroupoid
#M  HomogeneousGroupoidNC  
#M  HomogeneousDiscreteGroupoid 
##
InstallMethod( HomogeneousGroupoidNC, 
    "generic method for a connected gpd and lists of objects", true, 
    [ IsGroupoid, IsHomogeneousList ], 0,
function( gpd, oblist )

    local len, isos, inv1, pieces, hgpd, pisos; 

    
    len := Length( oblist );
    isos := List( oblist, L -> IsomorphismNewObjects( gpd, L ) ); 
    inv1 := InverseGeneralMapping( isos[1] ); 
    pieces := List( isos, f -> ImagesSource( f ) ); 
    hgpd := UnionOfPiecesOp( pieces, pieces[1] ); 
    pisos := List( [2..len], i -> inv1 * isos[i] );
    SetIsHomogeneousDomainWithObjects( hgpd, true ); 
    SetIsSinglePieceDomain( hgpd, false ); 
    SetPieceIsomorphisms( hgpd, pisos );
    SetObjectList( hgpd, Set( Flat( oblist ) ) ); 
    return hgpd; 
end );

InstallMethod( HomogeneousGroupoid, 
    "generic method for a connected gpd and lists of objects", true, 
    [ IsGroupoid, IsHomogeneousList ], 0,
function( gpd, oblist )

    local len, obs, ob1, j, L; 

    if not ForAll( oblist, L -> IsHomogeneousList(L) ) then 
        Error( "oblist must be a list of lists," ); 
    fi; 
    obs := gpd!.objects; 
    len := Length( obs ); 
    if not ForAll( oblist, L -> ( Length(L) = len ) ) then 
        Error( "lists in list must have the same length as Objects(gpd)," ); 
    fi; 
    ob1 := oblist[1]; 
    for j in [2..Length(oblist)] do 
        if ( Intersection( ob1, oblist[j] ) <> [ ] ) then
            Info( InfoGroupoids, 1, 
                  "constituents must have disjoint object sets," );
            return fail;
        fi; 
    od; 
    for L in oblist do 
        Sort( L );
    od;
    Sort( oblist );
    return HomogeneousGroupoidNC( gpd, oblist );
end );

InstallMethod( HomogeneousDiscreteGroupoid, 
    "generic method for a group and a list of objects", true, 
    [ IsGroup, IsHomogeneousList ], 0,
function( gp, obs ) 

    local fam, filter, gpd; 

    if ( Length( obs ) = 1 ) then 
        Error( "hom discrete groupoids should have more than one object" ); 
    fi;
    fam := IsGroupoidFamily; 
    filter := IsHomogeneousDiscreteGroupoidRep; 
    gpd := rec( objects := obs, magma := gp); 
    ObjectifyWithAttributes( gpd, IsHomogeneousDiscreteGroupoidType, 
        IsAssociative, true, 
        IsDiscreteDomainWithObjects, true, 
        IsHomogeneousDomainWithObjects, true, 
        IsAssociative, true, 
        IsCommutative, IsCommutative( gp ), 
        IsDirectProductWithCompleteDigraphDomain, false, 
        IsSinglePieceDomain, false ); 
    return gpd; 
end );


## ======================================================================= ##
##                           Groupoid Elements                             ##
## ======================================================================= ##

#############################################################################
##
#M  ArrowNC
#M  Arrow 
##
InstallOtherMethod( ArrowNC, 
    "for groupoid by isomorphisms, pair of elements, tail and head objects", 
    true, [ IsGroupoid, IsList, IsObject, IsObject ], 10, 
function( gpd, pair, t, h ) 

    local obs, elt, fam;

    fam := IsGroupoidElementFamily; 
    elt := Objectify( IsGroupoidByIsomorphismsElementType, 
                      [ pair, t, h, gpd ] ); 
    return elt; 
end ); 

InstallMethod( Arrow, "generic method for a groupoid element",
    true, [ IsGroupoid, IsMultiplicativeElement, IsObject, IsObject ], 0,
function( gpd, g, i, j ) 

    local comp, obs, ok1, ok2, rays, ri, rj;

    if ( HasIsSinglePiece( gpd ) 
         and IsSinglePiece( gpd ) ) then 
        comp := gpd; 
    else 
        comp := PieceOfObject( gpd, i );
    fi; 
    obs := comp!.objects; 
    ok1 := ( ( i in obs ) and ( j in obs ) ); 
    if ( HasIsDirectProductWithCompleteDigraph( comp ) 
         and IsDirectProductWithCompleteDigraph( comp ) ) then 
        ok2 := ( g in comp!.magma ); 
    else 
        rays := comp!.rays; 
        ri := rays[ Position( obs, i ) ]; 
        rj := rays[ Position( obs, j ) ];
        ok2 := ri * g * rj^(-1) in comp!.magma; 
    fi; 
    if not ( ok1 and ok2 ) then 
        return fail;
    else
        return ArrowNC( true, g, i, j ); 
    fi;
end );

InstallOtherMethod( Arrow, "generic method for a groupoid by isomorphisms",
    true, [ IsGroupoidByIsomorphisms, IsList, IsObject, IsObject ], 0,
function( gpd, pair, o1, o2 ) 

    local obs, ok1, p1, p2, gps, g1, g2, isos, iso, rays;

    Info( InfoGroupoids, 3, "Arrow: method for groupoid by isomorphisms" ); 
    obs := gpd!.objects; 
    ok1 := ( ( o1 in obs ) and ( o2 in obs ) ); 
    if not ok1 then 
        Info( InfoGroupoids, 2, "o1, o2 not both in gpd" ); 
        return fail; 
    fi;
    p1 := Position( obs, o1 ); 
    p2 := Position( obs, o2 );
    gps := ObjectGroups( gpd ); 
    g1 := pair[1]; 
    g2 := pair[2]; 
    isos := gpd!.isomorphisms; 
    iso := InverseGeneralMapping( isos[p1] ) * isos[p2]; 
    if not ( ImageElm( iso, g1 ) = g2 ) then 
        Info( InfoGroupoids, 2, "iso(g1) <> g2" );
        return fail; 
    else 
        return ArrowNC( gpd, pair, o1, o2 ); 
    fi;
end );

#############################################################################
## 
#M  ConjugateGroupoid( <gpd>, <elt> )
## 
InstallMethod( ConjugateGroupoid, "<gpd>, <elt>", true,
    [ IsGroupoid and IsSinglePiece, IsGroupoidElement ], 0, 
function( gpd, elt )

    local gens, ims, conj;

    gens := GeneratorsOfGroupoid( gpd ); 
    ims := List( gens, g -> g^elt ); 
    conj := SinglePieceSubgroupoidByGenerators( Ancestor( gpd ), ims );
    #? leftovers from the version for groups: 
    #? OnTuples( GeneratorsOfGroupoid( gpd ), elt ), One( gpd ) );
    #? UseIsomorphismRelation( gpd, conj );
    return conj;
end );

InstallMethod( ConjugateGroupoid, "<gpd>, <elt>", true,
    [ IsGroupoid and IsPiecesRep, IsGroupoidElement ], 0, 
function( U, elt )

    local p, q, np, nq, n, pieces, gpd; 

    p := elt![2]; 
    np := PieceNrOfObject( U, p ); 
    q := elt![3]; 
    nq := PieceNrOfObject( U, q ); 
    if ( np = fail ) then 
        if ( nq = fail ) then 
            return fail; 
        else 
            n := nq; 
        fi; 
    else 
        n := np; 
        if ( ( nq <> fail ) and ( np <> nq ) ) then 
            Info( InfoGroupoids, 1, "expecting np = nq here" ); 
        fi; 
    fi; 
    pieces := ShallowCopy( Pieces( U ) );
    gpd := pieces[n]; 
    pieces[n] := ConjugateGroupoid( gpd, elt ); 
    return UnionOfPieces( pieces ); 
end ); 

#############################################################################
##
#M  <e> in <G> 
##
InstallMethod( \in, "for groupoid element and a standard groupoid", true, 
    [ IsGroupoidElement, IsGroupoid and IsSinglePiece ], 0,
function( e, gpd )

    local obs, r1, r2, rays;

    obs := gpd!.objects; 
    if not ( (e![2] in obs) and (e![3] in obs) ) then 
        return false; 
    fi; 
    if ( HasIsDirectProductWithCompleteDigraph( gpd ) 
         and IsDirectProductWithCompleteDigraph( gpd ) ) then 
        return (e![1] in gpd!.magma);
    else 
        rays := gpd!.rays; 
        r1 := rays[ Position( obs, e![2] ) ]; 
        r2 := rays[ Position( obs, e![3] ) ]; 
        return ( r1 * e![1] * r2^(-1) in gpd!.magma ); 
    fi; 
end );

InstallMethod( \in, "for groupoid element and a groupoid by isomorphisms", 
    true, [ IsGroupoidElement, IsGroupoidByIsomorphisms ], 0,
function( e, gpd )

    local obs, rays, gps, isos, iso12, p1, p2;

    obs := gpd!.objects; 
    if not ( (e![2] in obs) and (e![3] in obs) ) then 
        return false; 
    fi; 
    if not ( e![4] = gpd ) then 
        return false; 
    fi; 
    rays := gpd!.rays; 
    isos := gpd!.isomorphisms; 
    gps := ObjectGroups( gpd ); 
    p1 := Position( obs, e![2] ); 
    p2 := Position( obs, e![3] ); 
    iso12 := InverseGeneralMapping( isos[p1] ) * isos[p2]; 
    return ( ( e![1][1] in gps[p1] ) and ( e![1][2] in gps[p2] ) 
             and ( ImageElm( iso12, e![1][1] ) = e![1][2] ) ); 
end );

InstallMethod( \in, "for groupoid element and a union of constituents", true, 
    [ IsGroupoidElement, IsGroupoid and HasPieces ], 0,
function( e, gpd )

    local p; 

    p := PieceOfObject( gpd, e![2] ); 
    if p = fail then 
        return false; 
    else 
        return e in p; 
    fi;
end );

#############################################################################
##
#M  PrintObj 
#M  Display
##
InstallMethod( PrintObj, "for a subset of elements of a groupoid", true, 
    [ IsHomsetCosets ], 0,
function ( hc )
        if ( hc!.type = "h" ) then 
        Print( "<homset ", hc!.tobs[1], " -> ", hc!.hobs[1],
               " with head group ", hc!.elements, ">" );
    elif ( hc!.type = "s" ) then 
        Print( "<star at ", hc!.tobs[1], 
               " with vertex group ", hc!.elements, ">" );
    elif ( hc!.type = "c" ) then 
        Print( "<costar at ", hc!.hobs[1], 
               " with vertex group ", hc!.elements, ">" );
    elif ( hc!.type = "r" ) then 
        Print( "<right coset of ", hc!.ActingDomain, 
               " with representative ", Representative( hc ),">" );
    elif ( hc!.type = "l" ) then 
        Print( "<left coset of ", hc!.ActingDomain, 
               " with representative ", Representative( hc ),">" );
    elif ( hc!.type = "d" ) then 
        Print( "<double coset of ", hc!.ActingDomain, 
               " with representative ", Representative( hc ),">" );
    else
        Print( "<object>");
    fi; 
end );

InstallMethod( Display, "for a subset of elements of a groupoid", true, 
    [ IsHomsetCosets ], 0,
function ( hc )
    
    local g; 

    if ( hc!.type = "h" ) then 
        Print( "<homset ", hc!.tobs[1], " -> ", hc!.hobs[1], 
               " with elements:\n" );
    elif ( hc!.type = "s" ) then 
        Print( "star at ", hc!.tobs[1], " with elements:\n" ); 
    elif ( hc!.type = "c" ) then 
        Print( "costar at ", hc!.hobs[1], " with elements:\n" );
    elif ( hc!.type = "r" ) then 
        Print( "<right coset of ", hc!.ActingDomain, " with elements:\n" );
    elif ( hc!.type = "l" ) then 
        Print( "<left coset of ", hc!.ActingDomain, " with elements:\n" );
    elif ( hc!.type = "d" ) then 
        Print( "<double coset of ", hc!.ActingDomain, " with elements:\n" );
    fi; 
    for g in hc do  
        Print( g, "\n" ); 
    od; 
end );

#############################################################################
##
#M  \=( <cset> ) . . . . . . . . . . . . . . . . . . . . for groupoid cosets 
##
InstallMethod( \=, "for groupoid cosets", [IsGroupoidCoset, IsGroupoidCoset], 
function( c1, c2 ) 

    local  act1, act2, sd1, sd2, rep1, rep2, type1, type2, elts1, elts2; 

    act1 := ActingDomain( c1 ); 
    act2 := ActingDomain( c2 ); 
    sd1 := SuperDomain( c1 ); 
    sd2 := SuperDomain( c2 ); 
    if not ( ( act1 = act2 ) and ( SuperDomain(c1) = SuperDomain(c2) ) ) then 
        Info( InfoGroupoids, 2, "different acting domain or super domain" ); 
        return false; 
    fi; 
    type1 := c1!.type; 
    type2 := c2!.type; 
    if not ( type1 = type2 ) then 
        Info( InfoGroupoids, 2, "different type" ); 
        return false; 
    fi; 
    rep1 := Representative( c1 ); 
    rep2 := Representative( c2 ); 
    if not ( rep1![2] = rep2![2] ) then 
        Info( InfoGroupoids, 2, "different tail" ); 
        return false; 
    fi; 
    if not ( rep1![3] = rep2![3] ) then 
        Info( InfoGroupoids, 2, "different head" ); 
        return false; 
    fi; 
    if not ( c1!.elements = c2!.elements ) then 
        Info( InfoGroupoids, 2, "different sets of elements" ); 
        return false; 
    fi;
    return true; 
end );

#############################################################################
##
#M  Size( <homsets> ) . . . . . . . . size for star, costar, homset or coset
##
InstallMethod( Size, "for a subset of a connected groupoid", 
    [ IsHomsetCosets ], 
function( hc ) 
    return Length(hc!.tobs) * Size( hc!.elements) * Length(hc!.hobs); 
end ); 

#############################################################################
##
#M  Iterator( <homsets> ) . . . . iterator for star, costar, homset or coset
##
InstallMethod( Iterator, "for a subset of a connected groupoid", 
    [ IsHomsetCosets ], 
function( hc )

    local elements, pro1, pro2;

    elements := hc!.elements; 
    return IteratorByFunctions( rec( 
        IsDoneIterator := function( iter )
            return ( IsDoneIterator( iter!.elementsIterator ) 
                     and ( iter!.tpos = iter!.tlen )
                     and ( iter!.hpos = iter!.hlen ) );
            end, 
        NextIterator := function( iter )
            if ( iter!.tpos = 0 ) then
                iter!.gpelt := NextIterator( iter!.elementsIterator );
                iter!.tpos := 1;
                iter!.hpos := 1;
           elif ((iter!.tpos = iter!.tlen) and (iter!.hpos = iter!.hlen)) then 
                iter!.gpelt := NextIterator( iter!.elementsIterator );
                iter!.tpos := 1;
                iter!.hpos := 1;
           elif ( iter!.hpos = iter!.hlen ) then
                iter!.hpos := 1;
                iter!.tpos := iter!.tpos + 1;
           else 
                iter!.hpos := iter!.hpos + 1;
            fi; 
            if ( hc!.type = "h" ) then 
                return ArrowNC( true, hc!.hrays[iter!.hpos]*(iter!.gpelt), 
                           iter!.tobs[iter!.tpos], iter!.hobs[iter!.hpos] );
            elif ( hc!.type = "c" ) then 
                return ArrowNC( true, (hc!.hrays[iter!.tpos])*(iter!.gpelt), 
                           iter!.tobs[iter!.tpos], iter!.hobs[iter!.hpos] );
            elif ( hc!.type = "s" ) then 
                return ArrowNC( true, (iter!.gpelt)*hc!.trays[iter!.hpos], 
                           iter!.tobs[iter!.tpos], iter!.hobs[iter!.hpos] );
            elif ( hc!.type = "r" ) then 
                return ArrowNC( true, 
                           (hc!.trays[iter!.tpos]^-1)*(iter!.gpelt),  
                           iter!.tobs[iter!.tpos], iter!.hobs[iter!.hpos] );
            elif ( hc!.type = "l" ) then 
                return ArrowNC( true, 
                           (iter!.gpelt)*(hc!.hrays[iter!.hpos]), 
                           iter!.tobs[iter!.tpos], iter!.hobs[iter!.hpos] );
            elif ( hc!.type = "d" ) then 
                return ArrowNC( true, 
                           hc!.trays[iter!.tpos]^-1
                            * (iter!.gpelt) 
                               * hc!.hrays[iter!.hpos],  
                           iter!.tobs[iter!.tpos], iter!.hobs[iter!.hpos] );
            fi; 
        end,
        ShallowCopy := iter -> 
            rec( elementsIterator := ShallowCopy( iter!.elementsIterator ), 
                 ## fgpd := iter!.fgpd, 
                 gpelt := iter!.gpelt,
                 tobs := iter!.tobs,
                 tlen := iter!.tlen,
                 hobs := iter!.hobs,
                 hlen := iter!.hlen,
                 tpos := iter!.tpos,
                 hpos := iter!.hpos,
                  rep := iter!.rep ),
        elementsIterator := Iterator( elements ), 
        ## fgpd := hc![1], 
        gpelt := 0,
        tobs := hc!.tobs,
        tlen := Length( hc!.tobs ),
        hobs := hc!.hobs,
        hlen := Length( hc!.hobs ),
        tpos := 0,
        hpos := 0, 
         rep := hc!.rep ) );
end );

#############################################################################
##
#M  ObjectStarNC 
#M  ObjectStar 
##
InstallMethod( ObjectStarNC, "for a connected groupoid and an object",
    true, [ IsGroupoid and IsSinglePiece, IsObject ], 0,
function( gpd, obj )

    local gp, obs, nobs, st, rays, pos, rpos;

    obs := gpd!.objects; 
    gp := ObjectGroup( gpd, obj ); 
    rays := gpd!.rays; 
    pos := Position( obs, obj ); 
    if ( pos <> 1 ) then  ## not the root object 
        rpos := rays[pos]^(-1); 
        rays := List( [1..Length(obs)], j -> rpos*rays[j] ); 
    fi; 
    st := rec( elements := gp, tobs := [ obj ], hobs := obs, 
               trays := rays, rep := (), type := "s" );
    ObjectifyWithAttributes( st, IsHomsetCosetsType, 
        IsHomsetCosets, true ); 
    return st;
end );

InstallMethod( ObjectStar, "generic method for a groupoid and an object",
    true, [ IsGroupoid, IsObject ], 0,
function( gpd, obj )

    local comp;

    if ( IsSinglePiece(gpd) and ( obj in ObjectList(gpd) ) ) then
        return ObjectStarNC( gpd, obj );
    else
        comp := PieceOfObject( gpd, obj );
        if ( comp = fail ) then
            Info( InfoGroupoids, 1, "obj not an object in gpd" );
            return fail;
        else
            return ObjectStarNC( comp, obj );
        fi;
    fi;
end );

#############################################################################
##
#M  ObjectCostarNC 
#M  ObjectCostar 
##
InstallMethod( ObjectCostarNC, "for a connected groupoid and an object",
    true, [ IsGroupoid and IsSinglePiece, IsObject ], 0,
function( gpd, obj )

    local gp, obs, nobs, cst, rays, pos, rpos;

    obs := gpd!.objects; 
    gp := ObjectGroup( gpd, obj ); 
    rays := gpd!.rays; 
    pos := Position( obs, obj ); 
    if ( pos <> 1 ) then  ## not the root object 
        rpos := rays[pos]; 
        rays := List( [1..Length(obs)], j -> rays[j]^(-1)*rpos ); 
    fi; 
    cst := rec( elements := gp, tobs := obs, hobs := [ obj ], 
                hrays := rays, rep := (), type := "c" ); 
    ObjectifyWithAttributes( cst, IsHomsetCosetsType, 
        IsHomsetCosets, true ); 
    return cst;
end );

InstallMethod( ObjectCostar, "generic method for a groupoid and an object",
    true, [ IsGroupoid, IsObject ], 0,
function( gpd, obj )

    local comp;

    if ( IsSinglePiece(gpd) and ( obj in ObjectList(gpd) ) ) then
        return ObjectCostarNC( gpd, obj );
    else
        comp := PieceOfObject( gpd, obj );
        if ( comp = fail ) then
            Info( InfoGroupoids, 1, "obj not an object in gpd" );
            return fail;
        else
            return ObjectCostarNC( comp, obj );
        fi;
    fi;
end );

#############################################################################
##
#M  HomsetNC 
#M  Homset
##
InstallMethod( HomsetNC, "for a connected groupoid and two objects",
    true, [ IsGroupoid and IsSinglePiece, IsObject, IsObject ], 0,
function( gpd, o1, o2 )

    local obs, rob, rays, gp, p1, p2, ray, hs;
    
    obs := gpd!.objects; 
    rob := obs[1]; 
    rays := RaysOfGroupoid( gpd );
    gp := ObjectGroup( gpd, o2 ); 
    if ( o1 = o2 ) then 
        return gp; 
    fi;
    p1 := Position( obs, o1 ); 
    p2 := Position( obs, o2 );
    if ( o1 = rob ) then 
        ray := rays[p2]; 
    elif ( o2 = rob ) then 
        ray := rays[p1]^(-1); 
    else 
        ray := rays[p1]^(-1) * rays[p2]; 
    fi;
    hs := rec( elements := gp, tobs := [ o1 ], hobs := [ o2 ], 
               hrays := [ ray ], rep := (), type := "h" ); 
    ObjectifyWithAttributes( hs, IsHomsetCosetsType, 
        IsHomsetCosets, true,
        Representative, ray ); 
    return hs;
end );

InstallMethod( Homset, "generic method for a groupoid and two objects",
    true, [ IsGroupoid, IsObject, IsObject ], 0,
function( gpd, o1, o2 )

    local obs, comp;

    obs := ObjectList( gpd );
    if not ( ( o1 in obs ) and ( o2 in obs ) ) then
        Info( InfoGroupoids, 1, "o1,o2 not objects in gpd" );
        return fail;
    fi;
    if IsSinglePiece( gpd ) then
        return HomsetNC( gpd, o1, o2 );
    else
        comp := PieceOfObject( gpd, o1 );
        if not ( o2 in comp!.objects ) then
            Info( InfoGroupoids, 1, "o1,o2 not objects in same constituent" );
            return fail;
        else
            return HomsetNC( comp, o1, o2 );
        fi;
    fi;
end );

#############################################################################
##
#M  ElementsOfGroupoid
##
InstallMethod( ElementsOfGroupoid, "for a connected groupoid", true,
    [ IsGroupoid and IsSinglePiece ], 0,
function( gpd )

    local iter, elts;

    elts := [ ];
    iter := Iterator( gpd );
    while not IsDoneIterator( iter ) do
        Add( elts, NextIterator( iter ) );
    od;
    return elts;
end );

InstallMethod( ElementsOfGroupoid, "generic method for a groupoid", true,
    [ IsGroupoid ], 0,
function( gpd )

    local comps, c, elts;
    comps := Pieces( gpd );
    elts := [ ];
    for c in comps do
        Append( elts, ElementsOfGroupoid( c ) );
    od;
    return elts;
end );

#############################################################################
##
#M  Iterator( <gpd> ) . . . . . . . . . . . . . . . . iterator for a groupoid
##
InstallMethod( Iterator, "for a connected groupoid", 
    [ IsGroupoid and IsSinglePiece ], 
function( gpd )
    return IteratorByFunctions( rec( 
        IsDoneIterator := function( iter )
            return ( IsDoneIterator( iter!.groupIterator ) 
                     and ( iter!.tpos = iter!.len )
                     and ( iter!.hpos = iter!.len ) );
            end, 
        NextIterator := function( iter )
            if ( iter!.tpos = 0 ) then
                iter!.gpelt := NextIterator( iter!.groupIterator );
                iter!.tpos := 1;
                iter!.hpos := 1;
            elif ((iter!.tpos = iter!.len) and (iter!.hpos = iter!.len)) then 
                iter!.gpelt := NextIterator( iter!.groupIterator );
                iter!.tpos := 1;
                iter!.hpos := 1;
            elif ( iter!.hpos = iter!.len ) then
                iter!.hpos := 1;
                iter!.tpos := iter!.tpos + 1;
            else 
                iter!.hpos := iter!.hpos + 1;
            fi;
            if ( HasIsDirectProductWithCompleteDigraph( gpd ) 
                 and IsDirectProductWithCompleteDigraph( gpd ) ) then 
                return ArrowNC( true, iter!.gpelt, 
                                iter!.obs[iter!.tpos], iter!.obs[iter!.hpos] );
            else 
                return ArrowNC( true, gpd!.rays[iter!.tpos]^(-1) 
                                      * iter!.gpelt * gpd!.rays[iter!.hpos], 
                                iter!.obs[iter!.tpos], iter!.obs[iter!.hpos] );
            fi; 
            end, 
        ShallowCopy := iter -> 
            rec( groupIterator := ShallowCopy( iter!.groupIterator ), 
                 gpelt := iter!.gpelt,
                 obs := iter!.obs,
                 len := iter!.len,
                 tpos := iter!.tpos,
                 hpos := iter!.hpos ),
        groupIterator := Iterator( gpd!.magma ), 
        gpelt := 0, 
        obs := gpd!.objects,
        len := Length( gpd!.objects ),
        tpos := 0,
        hpos := 0 ) );
end );

InstallMethod( Iterator, "generic method for a groupoid", [ IsGroupoid ], 
function( gpd )
    return IteratorByFunctions( rec( 
        IsDoneIterator := function( iter )
            return ( IsDoneIterator( iter!.groupoidIterator ) 
                     and ( iter!.cpos = iter!.len ) );
            end, 
        NextIterator := function( iter )
            if IsDoneIterator( iter!.groupoidIterator ) then 
                iter!.cpos := iter!.cpos + 1;
                iter!.groupoidIterator := 
                    Iterator( iter!.constituents[iter!.cpos] );
            fi;
            return NextIterator( iter!.groupoidIterator );
            end,
        ShallowCopy := iter -> 
            rec( constituents := iter!.constituents,
                 len := iter!.len,
                 groupoidIterator := ShallowCopy( iter!.groupoidIterator ),
                 cpos := iter!.cpos ),
        constituents := Pieces( gpd ),
        len := Length( Pieces( gpd ) ),
        groupoidIterator := Iterator( Pieces( gpd )[1] ),
        cpos := 1 ) );
end );

## ======================================================================= ##
##                                Subgroupoids                             ##
## ======================================================================= ##

#############################################################################
##
#F  Subgroupoid( <gpd>, <subgp> )        connected groupoids
#F  Subgroupoid( <gpd>, <gps>, <obs> )   discrete subgroupoid 
#F  Subgroupoid( <gpd>, <comp> )         subgroupoid as list of constituents
##
InstallGlobalFunction( Subgroupoid, function( arg )

    local nargs, gpd, id, rays, gp, sub; 

    nargs := Length( arg );
    gpd := arg[1];
    if not IsGroupoid( gpd ) then 
        Info( InfoGroupoids, 1, "arg[1] is not a groupoid" );
        return fail;
    fi;
    # by subgroup
    if ( ( nargs = 2 ) and IsSinglePiece( arg[1] )
                       and IsGroup( arg[2] ) ) then
        gp := gpd!.magma;
        Info( InfoGroupoids, 2, "connected subgroupoid" );
        sub := SinglePieceGroupoid( arg[2], gpd!.objects );
        SetParentAttr( sub, gpd );
        return sub;
    fi;
    # discrete subgroupoid
    if ( ( nargs = 3 ) and IsHomogeneousList( arg[2] ) 
                       and IsHomogeneousList( arg[3] ) ) then
        Info( InfoGroupoids, 2, "discrete subgroupoid" );
        return DiscreteSubgroupoid( arg[1], arg[2], arg[3] );
    fi;
    # list of constituents
    if ( ( nargs = 2 ) and IsHomogeneousList( arg[2] ) ) then
        Info( InfoGroupoids, 3, "subgroupoid by constituents" );
        return SubgroupoidByPieces( arg[1], arg[2] );
    fi;
    Info( InfoGroupoids, 1, SUB_CONSTRUCTORS );
    return fail;
end );

#############################################################################
##
#F  IsSubgroupoid( <G>, <U> )
##
InstallMethod( IsSubgroupoid, "generic method for two groupoids", true,
    [ IsGroupoid, IsGroupoid], 0,
function( G, U ) 
    ## Print( "IsSubgroupoid 1 : ", G, " >= ", U, "\n" ); 
    if ( HasParentAttr( U ) and ( ParentAttr( U ) = G ) ) then 
        return true;
    fi;
    if not IsSubset( ObjectList(G), ObjectList(U) ) then
        return false;
    fi; 
    return ForAll( Pieces( U ), C -> IsSubgroupoid( G, C ) );
end );

InstallMethod( IsSubgroupoid, "generic method for two groupoids", true,
    [ IsGroupoid, IsGroupoid and IsSinglePiece], 0,
function( G, U )
    ## Print( "IsSubgroupoid 2 : ", G, " >= ", U, "\n" ); 
    if ( HasParentAttr( U ) and ( ParentAttr( U ) = G ) ) then 
        return true;
    fi;
    if not IsSubset( ObjectList(G), ObjectList(U) ) then
        return false;
    fi; 
    return IsSubgroupoid( PieceOfObject(G,U!.objects[1]), U ); 
end ); 

InstallMethod( IsSubgroupoid, "generic method for two groupoids", true,
  [IsGroupoid and IsSinglePiece, IsGroupoid and IsSinglePiece], 0,
function( G, U )

    local objG, objU; 

    ## Print( "IsSubgroupoid 3 : ", G, " >= ", U, "\n" ); 
    if ( HasParentAttr( U ) and ( ParentAttr( U ) = G ) ) then 
        return true;
    fi;
    if not IsSubset( ObjectList(G), ObjectList(U) ) then
        return false;
    fi;  
    if ( G = U ) then 
        return true; 
    fi; 
    objG := G!.objects; 
    objU := U!.objects; 
    if not IsSubset( objG, objU ) then
        return false;
    fi; 
    if not IsSubgroup( ObjectGroup( G, objU[1] ), U!.magma ) then
        return false; 
    fi; 
    if not HasParentAttr( U ) then 
        SetParent( U, G ); 
    fi; 
    return true; 
end ); 

InstallMethod( IsWideSubgroupoid, "for two groupoids", true, 
    [ IsGroupoid, IsGroupoid ], 0, 
function( D, S )
    return ( IsSubgroupoid( D, S ) and 
             ObjectList( D ) = ObjectList( S ) ); 
end ); 

############################################################################
##
#M  SubgroupoidBySubgroup
##
InstallMethod( SubgroupoidBySubgroup, "for single piece groupoid and subgroup", 
    true, [ IsGroupoid and IsSinglePiece, IsGroup ], 0,
function( G, sgp ) 

    local gpG, U;     

    gpG := G!.magma; 
    if not IsSubgroup( gpG, sgp ) then 
        Error( "sgp is not a subgroup of RootGroup(G)" ); 
    fi; 
    if ( HasIsDirectProductWithCompleteDigraph( G ) 
         and IsDirectProductWithCompleteDigraph( G ) ) then 
        U := SinglePieceGroupoid( sgp, G!.objects );
    else
        U := SubgroupoidWithRays( G, sgp, RaysOfGroupoid( G ) ); 
    fi;
    SetParentAttr( U, G );
    return U; 
end ); 

#############################################################################
##
#M  SubgroupoidByPieces
##
InstallMethod( SubgroupoidByPieces,
    "generic method for a groupoid and a list of pieces", true,
    [ IsGroupoid, IsList ], 0, 
function( gpd, pieces )

    local p1, withrays, pieceU, len, i, pi, par, sub, U, c, piece, 
          gobs, grays, nobspi, j, ob, rays, rootpi, rootpos, obpos;

    p1 := pieces[1]; 
    withrays := ( ( "IsSinglePieceRaysRep" in RepresentationsOfObject(gpd) ) 
                  or ( Length( p1 ) = 3 ) )  
                and not ( ForAll( pieces, p -> Length( p[2] ) = 1 ) );  
    if IsList( p1 ) then 
        len := Length( pieces ); 
        pieceU := ListWithIdenticalEntries( len, 0 ); 
        for i in [1..len] do 
            pi := pieces[i]; 
            if withrays then 
                if not ( Length( pi ) = 3 ) then 
                    ## keep the rays of the larger gpd 
                    gobs := ObjectList( gpd ); 
                    grays := RaysOfGroupoid( gpd ); 
                    rays := ShallowCopy( pi[2] ); 
                    nobspi := Length( pi[2] );
                    rootpi := pi[2][1]; 
                    rootpos := Position( gobs, rootpi );
                    for j in [1..nobspi] do 
                        ob := pi[2][j]; 
                        obpos := Position( gobs, ob ); 
                        rays[j] := grays[rootpos]^(-1) * grays[obpos]; 
                    od;
                    par := SubgroupoidByObjects( gpd, pi[2] ); 
                    sub := SubgroupoidWithRays( par, pi[1], rays ); 
                else 
                    par := SubgroupoidByObjects( gpd, pi[2] ); 
                    sub := SubgroupoidWithRays( par, pi[1], pi[3] ); 
                fi; 
            else 
                sub := SinglePieceGroupoid( pi[1], pi[2] ); 
            fi; 
            pieceU[i] := sub; 
        od;
    else 
        pieceU := pieces; 
    fi;
    if ( Length( pieceU ) > 1 ) then
        U := UnionOfPieces( pieceU );
    else
        U := pieceU[1];
    fi;
    if not IsSubgroupoid( gpd, U ) then 
        Info( InfoGroupoids, 1, "union of pieces is not a subgroupoid of gpd" );
        return fail;
    fi;
    if ForAll( pieceU, p -> ( Length(p!.objects) = 1 ) ) then
        SetIsDiscreteDomainWithObjects( U, true );
    else
        SetIsDiscreteDomainWithObjects( U, false );
    fi;
    SetParentAttr( U, gpd );
    for c in pieceU do 
        piece := PieceOfObject( gpd, c!.objects[1] ); 
        SetParentAttr( c, SubgroupoidByObjects( piece, ObjectList(c) ) );
    od;
    return U;
end );

#############################################################################
##
#M  DiscreteSubgroupoid
##
InstallMethod( DiscreteSubgroupoid, "generic method for a groupoid", true,
    [ IsGroupoid, IsList, IsHomogeneousList ], 0,
function( G, gps, obs )

    local pieceU, U, len, o, pieceG, obsg, C, i, gpo, ishomo;

    obsg := ObjectList( G );
    len := Length( obs ); 
    if ( len = 1 ) then 
        return DomainWithSingleObject( gps[1], obs[1] ); 
    fi; 
    pieceU := ListWithIdenticalEntries( len, 0 );
    if not ( len = Length( gps ) ) then
        Error( "object and subgroup lists not of same length," );
    fi;
    for i in [1..len] do
        o := obs[i];
        if not ( o in obsg ) then
            Error( "i-th subgroupoid object not in groupoid," );
        fi;
        C := PieceOfObject( G, o ); 
        gpo := ObjectGroup( C, o ); 
        if not IsSubgroup( gpo, gps[i] ) then
            Error( "i-th group not a subgroup of the object group," );
        fi;
        pieceU[i] := DomainWithSingleObject( gps[i], o );
    od; 
    ishomo := ForAll( gps, g -> ( g = gps[1] ) ); 
    if ishomo then 
        Info( InfoGroupoids, 2, 
              "all groups equal, so using HomogeneousDiscreteGroupoid" ); 
        U := HomogeneousDiscreteGroupoid( gps[1], obs ); 
    else 
        U := UnionOfPieces( pieceU ); 
    fi;
    SetIsDiscreteDomainWithObjects( U, true );
    SetParentAttr( U, G );
    return U;
end );

#############################################################################
##
#M  SubgroupoidByObjects
##
InstallMethod( SubgroupoidByObjects, "for direct product groupoid + objects", 
    true, [ IsGroupoid and IsDirectProductWithCompleteDigraph, 
    IsHomogeneousList ], 10,
function( G, obsU )

    local obsG, nobs, U; 

    obsG := ObjectList( G ); 
    if not ForAll( obsU, o -> o in obsG ) then 
        Error( "<obsU> not a subset of the objects in G" ); 
    fi; 
    nobs := Length( obsU ); 
    if not ForAll( [1..nobs-1], j -> obsU[j] < obsU[j+1] ) then 
        Sort( obsU ); 
    fi; 
    U := SinglePieceGroupoidNC( G!.magma, obsU ); 
    SetParentAttr( U, G ); 
    return U;
end ); 

InstallMethod( SubgroupoidByObjects, "for a connected groupoid + object set", 
    true, [ IsGroupoid and IsSinglePieceRaysRep, IsHomogeneousList ], 0,
function( G, obsU )

    local obsG, nobs, ro, gpU, pos, rayG, rayU, ray1, j, anc; 

    obsG := ObjectList( G ); 
    if not ForAll( obsU, o -> o in obsG ) then 
        Error( "<obsU> not a subset of the objects in G" ); 
    fi; 
    nobs := Length( obsU ); 
    if not ForAll( [1..nobs-1], j -> obsU[j] < obsU[j+1] ) then 
        Sort( obsU ); 
    fi; 
    ro := obsU[1]; 
    gpU := ObjectGroup( G, ro ); 
    pos := List( [1..nobs], j -> Position ( obsG, obsU[j] ) ); 
    rayG := RaysOfGroupoid( G ); 
    rayU := ShallowCopy( obsU ); 
    ray1 := rayG[ pos[1] ]^(-1);  
    for j in [1..nobs] do 
        rayU[j] := ray1 * rayG[ pos[j] ]; 
    od; 
    anc := SubgroupoidByObjects( Ancestor( G ), obsU ); 
    return SubgroupoidWithRays( anc, gpU, rayU ); 
end );

InstallMethod( SubgroupoidByObjects, "for a groupoid and set of objects", 
    true, [ IsGroupoid, IsHomogeneousList ], 0,
function( G, sobs ) 

    local pieceG, pieceU, j, P, obsP, sobsP;

    pieceG := Pieces( G );
    pieceU := [ ];
    for j in [1..Length(pieceG)] do
        P := pieceG[j];
        obsP := P!.objects;
        sobsP := Intersection( sobs, obsP );
        if ( sobsP <> [ ] ) then 
            Add( pieceU, SubgroupoidByObjects( P, sobsP ) );
        fi;
    od;
    return UnionOfPieces( pieceU );
end );

InstallMethod( SubgroupoidByObjects, "for a homogeneous discrete groupoid", 
    true, [ IsHomogeneousDiscreteGroupoid, IsHomogeneousList ], 0,
function( gpd, sobs )
    local obs;
    obs := gpd!.objects; 
    if not ForAll( sobs, o -> o in obs ) then 
        Error( "<sobs> not a subset of <gpd>!.objects," ); 
    fi; 
    if ( Length( sobs ) = 1 ) then 
        return DomainWithSingleObject( gpd!.magma, sobs[1] ); 
    else 
        return HomogeneousDiscreteGroupoid( gpd!.magma, sobs ); 
    fi; 
end );

#############################################################################
##
#M  MaximalDiscreteSubgroupoid
#M  FullTrivialSubgroupoid
#M  DiscreteTrivialSubgroupoid
##
InstallMethod( MaximalDiscreteSubgroupoid,
    "generic method for a groupoid", true, [ IsGroupoid ], 0,
function( gpd )

    local obs, len, gps, i, piece, U;

    if ( HasIsDirectProductWithCompleteDigraph( gpd ) 
         and IsDirectProductWithCompleteDigraph( gpd ) ) then 
        U := HomogeneousDiscreteGroupoid( gpd!.magma, gpd!.objects ); 
    else 
        obs := ObjectList( gpd );
        len := Length( obs );
        gps := ListWithIdenticalEntries( len, 0 );
        for i in [1..len] do
            piece := PieceOfObject( gpd, obs[i] );
            gps[i] := ObjectGroup( piece, obs[i] ); 
        od;
        U := DiscreteSubgroupoid( gpd, gps, obs ); 
    fi;
    SetParentAttr( U, gpd ); 
    return U;
end );

InstallMethod( FullTrivialSubgroupoid, "for a connected groupoid", 
    true, [ IsGroupoid and IsSinglePiece ], 0,
function( gpd ) 

    local id, sub;

    id := TrivialSubgroup( gpd!.magma ); 
    if HasName( gpd!.magma ) then 
        SetName( id, Concatenation( "id(", Name( gpd!.magma ), ")" ) ); 
    fi; 
    sub := SinglePieceGroupoidNC( id, gpd!.objects );
    SetParentAttr( sub, gpd );
    return sub;
end );

InstallMethod( FullTrivialSubgroupoid, "generic method for a groupoid", 
    true, [ IsGroupoid ], 0,
function( gpd )

    local pieces, subs;

    pieces := Pieces( gpd );
    subs := List( pieces, c -> FullTrivialSubgroupoid(c) );
    return UnionOfPieces( subs );
end );

InstallMethod( DiscreteTrivialSubgroupoid, "for a connected groupoid", 
    true, [ IsGroupoid and IsSinglePiece ], 0,
function( gpd )

    local id, obs, gps;

    obs := gpd!.objects;
    id := TrivialSubgroup( gpd!.magma );
    if HasName( gpd!.magma ) then 
        SetName( id, Concatenation( "id(", Name( gpd!.magma ), ")" ) ); 
    fi; 
    gps := List( obs, o -> id );
    return DiscreteSubgroupoid( gpd, gps, obs );
end );

InstallMethod( DiscreteTrivialSubgroupoid, "for a connected groupoid", 
    true, [ IsGroupoid and IsDiscreteDomainWithObjects ], 0,
function( gpd )
    return FullTrivialSubgroupoid( gpd );
end );

InstallMethod( DiscreteTrivialSubgroupoid, "generic method for a groupoid", 
    true, [ IsGroupoid ], 0,
function( gpd )

    local pieces, subs;

    pieces := Pieces( gpd );
    subs := List( pieces, c -> DiscreteTrivialSubgroupoid(c) );
    return UnionOfPieces( subs );
end );

#############################################################################
##
#M  IsNormalSubgroupoid 
##
InstallMethod( IsNormalSubgroupoid, "for two subgroupoids", true, 
    [ IsGroupoid and IsSinglePiece, IsGroupoid ], 0,
function( G, N )

    local gpG, gpN; 

    if not IsWideSubgroupoid( G, N ) then 
        return false; 
    fi; 
    if not IsNormal( G!.magma, N!.magma ) then 
        return false; 
    fi;
    if IsSinglePiece( N ) then 
        return true; 
    elif IsDiscreteDomainWithObjects( N ) then 
        return true; 
    else 
        return false; 
    fi;
end ); 

#############################################################################
##
#M  ReplaceOnePieceInUnion 
##
InstallOtherMethod( ReplaceOnePieceInUnion, "for union, posint and gpd", true, 
    [ IsGroupoid and IsPiecesRep, IsPosInt, IsGroupoid and IsSinglePiece ], 0,
function( U, pos, new )

    local pieces; 

    pieces := ShallowCopy( Pieces( U ) ); 
    if not ( pos <= Length(pieces) ) then 
        Info( InfoGroupoids, 1, "U has fewer than pos pieces" ); 
        return fail; 
    fi; 
    ## OK to replace old by new in the pn-th place 
    pieces[pos] := new; 
    return UnionOfPieces( pieces ); 
end ); 

InstallMethod( ReplaceOnePieceInUnion, "for union and two gpds", true, 
    [ IsGroupoid and IsPiecesRep, IsGroupoid and IsSinglePiece, 
      IsGroupoid and IsSinglePiece ], 0,
function( U, old, new )

    local pieces, pos; 

    pieces := ShallowCopy( Pieces( U ) ); 
    pos := Position( pieces, old ); 
    if ( pos = fail ) then 
        Info( InfoGroupoids, 1, "old not a piece in U" ); 
        return fail; 
    fi; 
    ## OK to replace old by new in the pn-th place 
    pieces[pos] := new; 
    return UnionOfPieces( pieces ); 
end ); 

#############################################################################
##
#M  PiecePositions
##
InstallMethod( PiecePositions, "for a groupoid and a subgroupoid", true,
    [ IsGroupoid, IsGroupoid ], 0,
function( G, U )

    local obsG, obU, lenG, lenU, pos, i, oi, j, found, p;

    if IsSinglePiece( G ) then
        if IsSinglePiece( U ) then
            pos := [ 1 ];
        else
            pos := List( Pieces(U), c -> 1 );
        fi;
    else
        obsG := List( Pieces(G), c -> c!.objects );
        lenG := Length( obsG );
        obU := List( Pieces(U), c -> c!.objects[1] );
        lenU := Length( obU );
        pos := ListWithIdenticalEntries( lenU, 0 );
        for i in [1..lenU] do
            oi := obU[i];
            found := false;
            j := 0;
            while ( not found ) do
                j := j+1;
                p := Position( obsG[j], oi );
                if ( p <> fail ) then
                    pos[i] := j;
                    found := true;
                fi;
            od;
        od;
    fi;
    Info( InfoGroupoids, 3, "constituent positions = ", pos ); 
    return pos;
end );


## ======================================================================== ##
##                            Groupoid Cosets                               ##
## ======================================================================== ##

##############################################################################
##
#M  RightCoset 
#M  LeftCoset
#M  DoubleCoset
##
#? (24/09/08)  decided to include the larger groupoid - a mistake ?? 
## (24/09/08)  could split this into a non-NC and an NC operation -
##             but there is no RightCosetNC in the library 
#? (03/10/08)  now allowing U not to be wide in G (see RightCosets). 

InstallOtherMethod( RightCoset, "for groupoid, subgroupoid and element", 
    true, [ IsGroupoid, IsGroupoid, IsMultiplicativeElementWithObjects ], 0, 
function( gpd, sgpd, e ) 

    local G, U, obsU, nobsU, root, rayU, arayU, r, gpt, rc, rcos; 

    if not IsSubgroupoid( gpd, sgpd ) then
        Error( "sgpd not a subgroupoid of gpd," );
    fi;
    if IsSinglePiece( gpd ) then 
        G := gpd; 
    else
        Info( InfoGroupoids, 2, "comment: gpd is not a single piece!" ); 
        G := PieceOfObject( gpd, e![2] ); 
        if ( G = fail ) then 
            Error( "tail of e is not an object in gpd" ); 
        fi; 
    fi; 
    if IsSinglePiece( sgpd ) then 
        U := sgpd; 
        if not ( e![2] in ObjectList( U ) ) then
            Error( "tail of e not in U" ); 
        fi; 
    else
        Info( InfoGroupoids, 2, "comment: sgpd is not a single piece!" ); 
        U := PieceOfObject( sgpd, e![2] ); 
        if ( U = fail ) then 
            Error( "tail of e is not an object in sgpd" ); 
        fi; 
    fi; 
    if not IsSubdomainWithObjects( G, U ) then 
        Error( "U not a subgroupoid of G," ); 
    fi; 
    obsU := ObjectList( U );
    nobsU := Length( obsU ); 
    root := obsU[1]; 
    rayU := RaysOfGroupoid( U ); 
    arayU := RayArrowsOfGroupoid( U ); 
    ##  choose a representative arrow from the root object 
    r := arayU[ Position( obsU, e![2] ) ] * e;  
    gpt := ObjectGroup( U, root ); 
    rc := RightCoset( gpt, r![1] ); 
    rcos := rec( elements := rc, tobs := obsU, hobs := [ e![3] ], 
                 rep := r![1], trays := rayU, type := "r" ); 
    ObjectifyWithAttributes( rcos, IsHomsetCosetsType, 
        IsGroupoidCoset, true, 
        SuperDomain, G,
        ActingDomain, U,  
        Size, Size( gpt ) * nobsU, 
        Representative, r ); 
    return rcos; 
end ); 

InstallOtherMethod( LeftCoset, "for groupoid, subgroupoid and element", 
    true, [ IsGroupoid, IsGroupoid, IsGroupoidElement ], 0, 
function( gpd, sgpd, e ) 

    local G, V, obsV, nobsV, root, rayV, arayV, rays, r, gph, lc, lcos; 

    if not IsSubgroupoid( gpd, sgpd ) then
        Error( "sgpd not a subgroupoid of gpd," );
    fi;
    if IsSinglePiece( gpd ) then 
        G := gpd; 
    else
        Info( InfoGroupoids, 2, "comment: gpd not a single piece!" ); 
        G := PieceOfObject( gpd, e![3] ); 
        if ( G = fail ) then 
            Error( "head of e is not an object in gpd" ); 
        fi; 
    fi; 
    if IsSinglePiece( sgpd ) then 
        V := sgpd; 
        if not ( e![3] in ObjectList( V ) ) then
            Error( "head of e not in V" ); 
        fi; 
    else
        Info( InfoGroupoids, 2, "comment: sgpd is not a single piece!" ); 
        V := PieceOfObject( sgpd, e![3] ); 
        if ( V = fail ) then 
            Error( "head of e is not an object in sgpd" ); 
        fi; 
    fi; 
    if not IsSubdomainWithObjects( G, V ) then 
        Error( "V not a subgroupoid of G," ); 
    fi; 
    obsV := ObjectList( V );
    nobsV := Length( obsV ); 
    root := obsV[1]; 
    rayV := RaysOfGroupoid( V );
    arayV := RayArrowsOfGroupoid( V );
    ##  choose a representative arrow from the root object 
    r := e * arayV[ Position( obsV, e![3] ) ]^-1;  
    gph := ObjectGroup( V, r![3] ); 
    lc := List( RightCoset( gph, r![1]^-1 ), g -> g^-1 ); 
    lcos := rec( elements := lc, tobs := [ e![2] ], hobs := obsV, 
                 rep := r![1], hrays := rayV, type := "l" ); 
    ObjectifyWithAttributes( lcos, IsHomsetCosetsType, 
        IsGroupoidCoset, true, 
        SuperDomain, G,
        ActingDomain, V, 
        Size, Size( gph ) * nobsV, 
        Representative, r ); 
    return lcos;
end ); 

InstallOtherMethod( DoubleCoset, "for groupoid, two subgroupoids, and element", 
    true, [ IsGroupoid, IsGroupoid, IsGroupoid, IsGroupoidElement ], 0, 
function( gpd, lsgpd, rsgpd, e ) 

    local G, U, V, obsU, nobsU, rootU, rayU, arayU, rt, gpt, 
          obsV, nobsV, rootV, rayV, arayV, rh, gph, r, dc, dcos; 

    if not ( IsSubgroupoid( gpd, lsgpd ) and IsSubgroupoid( gpd, rsgpd ) ) then
        Error( "one of lsgpd and rsgpd not a subgroupoid of gpd" );
    fi; 
    if not ( e in gpd ) then 
        Error( "arrow e is not in gpd" ); 
    fi; 
    if IsSinglePiece( gpd ) then 
        G := gpd; 
    else
        Info( InfoGroupoids, 2, "comment: gpd not a single piece!" ); 
        G := PieceOfObject( gpd, e![2] ); 
        if ( G = fail ) then 
            Error( "tail of e is not an object in gpd" ); 
        fi; 
    fi; 
    if IsSinglePiece( lsgpd ) then 
        U := lsgpd; 
    else
        Info( InfoGroupoids, 2, "comment: lsgpd not a single piece!" ); 
        U := PieceOfObject( lsgpd, e![2] ); 
        if ( U = fail ) then 
            Error( "tail of e is not an object in lsgpd" ); 
        fi; 
    fi; 
    if not IsSubdomainWithObjects( G, U ) then 
        Error( "U not a subgroupoid of G," ); 
    fi; 
    if IsSinglePiece( rsgpd ) then 
        V := rsgpd; 
    else
        Info( InfoGroupoids, 2, "comment: rsgpd not a single piece!" ); 
        V := PieceOfObject( rsgpd, e![3] ); 
        if ( V = fail ) then 
            Error( "head of e is not an object in rsgpd" ); 
        fi; 
    fi; 
    if not IsSubdomainWithObjects( G, V ) then 
        Error( "V not a subgroupoid of G," ); 
    fi; 
    ## end of checks 

    obsU := ObjectList( U );
    nobsU := Length( obsU ); 
    rootU := obsU[1]; 
    rayU := RaysOfGroupoid( U ); 
    arayU := RayArrowsOfGroupoid( U );
    rt := arayU[ Position( obsU, e![2] ) ];  
    gpt := ObjectGroup( U, rootU );

    obsV := ObjectList( V );
    nobsV := Length( obsV ); 
    rootV := obsV[1]; 
    rayV := RaysOfGroupoid( V );
    arayV := RayArrowsOfGroupoid( V ); 
    rh := arayV[ Position( obsV, e![3] ) ]^-1; 
    gph := ObjectGroup( V, rootV ); 

    r := rt * e * rh; 
    dc := DoubleCoset( gpt, r![1], gph ); 
    dcos := rec( elements := dc, tobs := obsU, hobs := obsV, 
                 rep := r![1], trays := rayU, hrays := rayV, type := "d" ); 
    ObjectifyWithAttributes( dcos, IsHomsetCosetsType, 
        IsGroupoidCoset, true, 
        SuperDomain, G,
        ActingDomain, [U,V], 
        Size, nobsU * Size( dc ) * nobsV, 
        Representative, r ); 
    return dcos;
end ); 

InstallMethod( PrintObj, "RightCoset", true, 
    [ IsGroupoidCoset and IsRightCosetDefaultRep ], 0,
function( r )
  Print( "RightCoset(", ActingDomain(r), ",", r!.rep, ")" );
##  Print( "RightCoset(", ActingDomain(r), ",", Representative(r), ")" );
end); 

InstallMethod( ViewObj, "RightCoset", true, 
    [ IsGroupoidCoset and IsRightCosetDefaultRep ], 0,
function( r )
    Print( "RightCoset(", ActingDomain(r), ",", r!.rep, ")" );
##    Print( "RightCoset(", ActingDomain(r), ",", Representative(r), ")" );
end ); 

InstallMethod( PrintObj, "LeftCoset", true, 
    [ IsGroupoidCoset and IsLeftCosetWithObjectsDefaultRep ], 0,
function( l )
    Print( "LeftCoset(", Representative(l), ",", ActingDomain(l), ")" );
end ); 

InstallMethod( ViewObj, "LeftCoset", true, 
    [ IsGroupoidCoset and IsLeftCosetWithObjectsDefaultRep ], 0,
function( l )
    Print( "LeftCoset(", Representative(l), ",", ActingDomain(l), ")" );
end ); 

InstallMethod( \in, "for stars, costars, homsets, cosets, etc.", true, 
    [ IsGroupoidElement, IsHomsetCosets ], 0,
function( e, hc ) 

    local r, pos, rep; 

    if ( hc!.type = "h" ) then ## homset 
        if ( e![2] <> hc!.tobs[1] ) then return false; fi; 
        if ( e![3] <> hc!.hobs[1] ) then return false; fi; 
        if not ( hc!.hrays[1]^(-1)*e![1] in hc!.elements ) then 
            return false; 
        fi; 
        return true;
    elif ( hc!.type = "s" ) then ## star  
        if ( e![2] <> hc!.tobs[1] ) then return false; fi; 
        pos := Position( hc!.hobs, e![3] );
        if ( pos = fail ) then return false; fi; 
        r := hc!.trays[pos]^-1; 
        if not ( e![1]*r in hc!.elements ) then return false; fi; 
        return true;
    elif ( hc!.type = "c" ) then ## costar  
        if ( e![3] <> hc!.hobs[1] ) then return false; fi; 
        pos := Position( hc!.tobs, e![2] );
        if ( pos = fail ) then return false; fi; 
        r := hc!.hrays[pos]^-1; 
        if not ( r*e![1] in hc!.elements ) then return false; fi; 
        return true;
    elif ( hc!.type = "r" ) then ## right coset 
        rep := hc!.rep; 
        if ( e![3] <> hc!.hobs[1] ) then return false; fi; 
        pos := Position( hc!.tobs, e![2] ); 
        if ( pos = fail ) then return false; fi; 
        r := hc!.trays[pos] * e![1]; 
        if not ( r in hc!.elements ) then return false; fi; 
        return true; 
    elif ( hc!.type = "l" ) then ## left coset 
        rep := hc!.rep; 
        if ( e![2] <> hc!.tobs[1] ) then return false; fi; 
        pos := Position( hc!.hobs, e![3] ); 
        if ( pos = fail ) then return false; fi; 
        r := e![1] * hc!.hrays[pos]^-1; 
        if not ( r in hc!.elements ) then return false; fi; 
        return true; 
    elif ( hc!.type = "d" ) then ## double coset   
        Error( "'in' not yet implemented for double cosets" );
    else 
        Error( "type of HomsetCosets not recognised" ); 
    fi;
end );

#############################################################################
########  Why not `IsIdenticalObj' in the following declarations ??? ########

############################################################################# 
## 
#M  RightCosetRepresentatives
## 
InstallMethod( RightCosetRepresentatives, "generic method for a subgroupoid",
    true, [ IsGroupoid and IsSinglePiece, IsGroupoid ], 0,
function( G, U )

    local gpG, obG, nobG, reps, P, obP, gpP, nrc, o, rco, r, ro, obs;

    if not IsSubgroupoid( G, U ) then
        Error( "U not a subgroupoid of G," );
    fi;
    gpG := G!.magma;
    obG := G!.objects;
    nobG := Length( obG );
    reps := [ ];
    for P in Pieces( U ) do
        obP := P!.objects; 
        gpP := P!.magma;
        nrc := Size( gpG )/Size( gpP );
        for o in obP do 
            rco := RightCosets( ObjectGroup( G, o ), ObjectGroup( P, o ) ); 
            for r in rco do
                Add( reps, ArrowNC( true, Representative(r), o, o ) );
            od;
        od;
        obs := Difference( obG, obP );
        ro := obP[1]; 
        rco := RightCosets( ObjectGroup( G, ro ), ObjectGroup( P, ro ) ); 
        for o in obs do 
            for r in rco do
                Add( reps, ArrowNC( true, Representative(r), ro, o ) );
            od;            
        od; 
    od;
    return reps;
end ); 

InstallMethod( RightCosetRepresentatives, "generic method for a subgroupoid",
    true, [ IsGroupoid, IsGroupoid ], 0,
function( G, U )

    local pos, cG, cU, ncU, reps, i, filt, piece, m, j, subU;

    if not IsSubgroupoid( G, U ) then
        Error( "U not a subgroupoid of G," );
    fi;
    pos := PiecePositions( G, U );
    reps := [ ];
    cG := Pieces( G );
    cU := Pieces( U );
    ncU := Length( cU );
    for i in [1..Length(cG)] do
        filt := Filtered( [1..ncU], p -> ( pos[p] = i ) );
        piece := List( filt, p -> cU[p] );
        if ( Length(filt) = 1 ) then
            subU := piece[1];
        else
            subU := UnionOfPiecesOp( piece, piece[1] );
        fi;
        Append( reps, RightCosetRepresentatives( cG[i], subU ) );
    od;
    return reps;
end );

############################################################################# 
## 
#M  LeftCosetRepresentatives
## 
InstallMethod( LeftCosetRepresentatives, "generic method for a subgroupoid",
    true, [ IsGroupoid and IsSinglePiece, IsGroupoid ], 0,
function( G, U )

    local gpG, obG, nobG, reps, P, obP, gpP, nlc, o, rco, r, rep, ro, obs;

    if not IsSubgroupoid( G, U ) then
        Error( "U not a subgroupoid of G," );
    fi;
    gpG := G!.magma; 
    obG := G!.objects;
    nobG := Length( obG );
    reps := [ ];
    for P in Pieces( U ) do
        obP := P!.objects;
        gpP := P!.magma; 
        nlc := Size( gpG )/Size( gpP ); 
        for o in obP do 
            rco := RightCosets( ObjectGroup( G, o ), ObjectGroup( P, o ) );
            for r in rco do 
                rep := Representative( r ); 
                Add( reps, ArrowNC( true, rep^(-1), o, o ) ); 
            od; 
        od;
        obs := Difference( obG, obP ); 
        ro := obP[1]; 
        rco := RightCosets( ObjectGroup( G, ro ), ObjectGroup( P, ro ) );
        for o in obs do 
            for r in rco do 
                rep := Representative( r ); 
                Add( reps, ArrowNC( true, rep^(-1), o, ro ) ); 
            od; 
        od; 
    od;
    return reps;
end );    

InstallMethod( LeftCosetRepresentatives, "generic method for a subgroupoid",
    true, [ IsGroupoid, IsGroupoid ], 0,
function( G, U )

    local pos, cG, cU, ncU, reps, i, filt, piece, m, j, subU;

    if not IsSubgroupoid( G, U ) then
        Error( "U not a subgroupoid of G," );
    fi;
    pos := PiecePositions( G, U );
    reps := [ ];
    cG := Pieces( G );
    cU := Pieces( U );
    ncU := Length( cU );
    for i in [1..Length(cG)] do
        filt := Filtered( [1..ncU], p -> ( pos[p] = i ) );
        piece := List( filt, p -> cU[p] );
        if ( Length(filt) = 1 ) then
            subU := piece[1];
        else
            subU := UnionOfPiecesOp( piece, piece[1] );
        fi;
        Append( reps, LeftCosetRepresentatives( cG[i], subU ) );
    od;
    return reps;
end );

############################################################################# 
## 
#M  LeftCosetRepresentativesFromObject
## 
InstallMethod( LeftCosetRepresentativesFromObject, "for a subgroupoid",
    true, [ IsGroupoid and IsSinglePiece, IsGroupoid, IsObject ], 0,
function( G, U, o )

    local gpG, obG, reps, P, obP, gpP, rco, obs, r, rep, ro;

    if not IsWideSubgroupoid( G, U ) then
        Error( "U not a wide subgroupoid of G," );
    fi;
    gpG := G!.magma;
    obG := G!.objects;
    if not ( o in obG ) then
        Error( "chosen object not in the groupoid," );
    fi;
    reps := [ ];
    for P in Pieces( U ) do
        obP := P!.objects;
        gpP := P!.magma;
        obs := Difference( obG, obP ); 
        if o in obP then 
            rco := RightCosets( ObjectGroup( G, o ), ObjectGroup( P, o ) ); 
            for r in rco do 
                rep := Representative( r ); 
                Add( reps, ArrowNC( true, rep^(-1), o, o ) ); 
            od;
        else 
            ro := obP[1]; 
            rco := RightCosets( ObjectGroup( G, ro ), ObjectGroup( P, ro ) ); 
            for r in rco do 
                rep := Representative( r ); 
                Add( reps, ArrowNC( true, rep^(-1), o, ro ) ); 
            od; 
        fi; 
    od;
    return reps;
end );    

InstallMethod( LeftCosetRepresentativesFromObject, "for a subgroupoid",
    true, [ IsGroupoid, IsGroupoid, IsObject ], 0,
function( G, U, o )

    local pos, cG, co, i, cU, ncU, filt, piece, m, j, subU;

    if not IsWideSubgroupoid( G, U ) then
        Error( "U not a wide subgroupoid of G," );
    fi;
    pos := PiecePositions( G, U );
    cG := Pieces( G );
    co := PieceOfObject( G, o );
    if ( co = fail ) then
        Error( "chosen object not in the groupoid," );
    fi;
    i := Position( cG, co );
    cU := Pieces( U );
    ncU := Length( cU );
    filt := Filtered( [1..ncU], p -> ( pos[p] = i ) );
    piece := List( filt, p -> cU[p] );
    if ( Length(filt) = 1 ) then
        subU := piece[1];
    else
        subU := UnionOfPiecesOp( piece, piece[1] );
    fi;
    return LeftCosetRepresentativesFromObject( co, subU, o );
end );

############################################################################# 
## 
#M  DoubleCosetRepresentatives
## 
InstallMethod( DoubleCosetRepresentatives, "generic method for 2 subgroupoids",
    true, [ IsGroupoid and IsSinglePiece, IsGroupoid, IsGroupoid ], 0,
function( G, U, V )

    local gg, obG, nobG, reps, cu, ou, gu, cv, ov, gv, dc, c, r;

    if not ( IsWideSubgroupoid( G, U ) and IsWideSubgroupoid( G, V ) ) then
        Error( "U,V not wide subgroupoids of G," );
    fi;
    gg := G!.magma;
    obG := G!.objects;
    nobG := Length( obG );
    reps := [ ];
    for cu in Pieces( U ) do
        ou := cu!.objects[1];
        gu := cu!.magma;
        for cv in Pieces( V ) do
            ov := cv!.objects[1];
            gv := cv!.magma;
            dc := DoubleCosets( gg, gu, gv );
            for c in dc do
                r := Representative( c );
                Add( reps, ArrowNC( true, r, ou, ov ) );
            od;
        od;
    od;
    return reps;
end );    

##  still need to implement  DoubleCosetRepresentatives for G not connected

InstallMethod( DoubleCosetRepresentatives, "generic method for 2 subgroupoids",
    true, [ IsGroupoid, IsGroupoid, IsGroupoid ], 0,
function( G, U, V )

    local posU, posV, cG, cU, ncU, cV, ncV, reps, i, filt, piece, m, j, subU;

    if not IsSubgroupoid( G, U ) then
        Error( "U not a subgroupoid of G," );
    fi;
    posU := PiecePositions( G, U ); 
    posV := PiecePositions( G, V ); 
    reps := [ ];
    cG := Pieces( G );
    cU := Pieces( U );
    ncU := Length( cU );
    cV := Pieces( V );
    ncV := Length( cV ); 
    Print( "DoubleCosetReps not yet implemented when G not connected\n" );
    return fail;
end );



#############################################################################
##
#M  RightCosetsNC 
#M  LeftCosetsNC 
#M  DoubleCosetsNC 
##
##  right cosets refine costars and left cosets refine stars 
##
##  RightCosets(G,U) is normally defined only when U is wide in G. 
##  For computational purposes we shall use, for the more general case, 
##  RightCosets(F,U) where F is the SubgroupoidByObjects of G on Objects(U). 
##
InstallOtherMethod( RightCosetsNC, "for groupoids", true,
    [ IsGroupoid, IsGroupoid ], 0,
function( G, U )

    local cosets, reps, nr, i, e;

    reps := RightCosetRepresentatives( G, U );
    nr := Length( reps );
    cosets := ListWithIdenticalEntries( nr, 0 );
    for i in [1..nr] do
        e := reps[i];
        cosets[i] := RightCoset( G, U, e );
    od;
    return cosets;
end);
    
InstallOtherMethod( LeftCosetsNC, "for groupoids", true, 
    [ IsGroupoid, IsGroupoid ], 0,
function( G, U )

    local cosets, reps, nr, i, e;

    reps := LeftCosetRepresentatives( G, U );
    nr := Length( reps );
    cosets := ListWithIdenticalEntries( nr, 0 );
    for i in [1..nr] do
        e := reps[i];
        cosets[i] := LeftCoset( G, U, e );
    od;
    return cosets;
end);
    
InstallOtherMethod( DoubleCosetsNC, "for groupoids", true,
    [ IsGroupoid, IsGroupoid, IsGroupoid ], 0,
function( G, U, V )

    local cosets, reps, nr, i, e;

    reps := DoubleCosetRepresentatives( G, U, V );
    nr := Length( reps );
    cosets := ListWithIdenticalEntries( nr, 0 );
    for i in [1..nr] do
        e := reps[i];
        cosets[i] := DoubleCoset( G, U, V, e );
    od;
    return cosets;
end);
    
#############################################################################
##
#M  \*( <e1>, <e2> )  . . . . . . . . . . .  product of two groupoid elements
#M  \^( <e>, <n> )  . . . . . . . . . . . . . . . power of a groupoid element
#M  \^( <e1>, <e2> )  . . . . .  conjugate of one groupoid element by another
##
InstallMethod( \*, "for two groupoid elements", IsIdenticalObj,
    [ IsGroupoidElement, IsGroupoidElement ], 0,
function( e1, e2 )
    if ( e1![3] <> e2![2] ) then
        return fail;
    fi; 
    return ArrowNC( true, e1![1]*e2![1], e1![2], e2![3] );
end );

InstallMethod( \*, "for two groupoid by isomorphisms elements", IsIdenticalObj,
    [ IsGroupoidByIsomorphismsElement, IsGroupoidByIsomorphismsElement ], 0,
function( e1, e2 )

    local u, v, w, gpd, obs, pu, pv, pw, isos, invuv, isovw, prod, g1, g2; 

    u := e1![2]; 
    v := e1![3]; 
    w := e2![3]; 
    gpd := e1![4]; 
    if ( ( v <> e2![2] ) and ( gpd <> e2![4] ) ) then
        return fail;
    fi; 
    isos := gpd!.isomorphisms; 
    obs := gpd!.objects; 
    pu := Position( obs, u ); 
    pv := Position( obs, v ); 
    pw := Position( obs, w ); 
    invuv := InverseGeneralMapping( isos[pv] ) * isos[pu]; 
    isovw := InverseGeneralMapping( isos[pv] ) * isos[pw];
    prod := e1![1][2] * e2![1][1]; 
    g1 := ImageElm( invuv, prod ); 
    g2 := ImageElm( isovw, prod ); 
    return Arrow( gpd, [ g1, g2 ], e1![2], e2![3] ); 
end );

InstallMethod( \^, "for a groupoid element and an integer", true,
    [ IsGroupoidElement, IsInt ], 0,
function( e, n )

    local t, h;

    t := e![2];
    h := e![3];
    if ( n = 1 ) then
        return e;
    elif ( n = -1 ) then
        return ArrowNC( true, e![1]^-1, h, t );
    elif ( t = h ) then
        return ArrowNC( true, e![1]^n, t, h );
    fi; 
    return fail; 
end );

InstallMethod( \^, "for a groupoid by isomorphisms element and an integer", 
    true, [ IsGroupoidByIsomorphismsElement, IsInt ], 0,
function( e, n )

    local t, h, gpd, isos;

    t := e![2];
    h := e![3];
    gpd := e![4]; 
    isos := gpd!.isomorphisms; 
    if ( n = 1 ) then
        return e;
    elif ( n = -1 ) then
        return Arrow( gpd, [ e![1][2]^(-1), e![1][1]^(-1) ], h, t );
    elif ( t = h ) then
        return Arrow( gpd, [ e![1][1]^n, e![1][2]^n ], t, h );
    fi; 
    return fail; 
end );


##  This operator follows the definitions in Alp/Wensley 2010

InstallMethod( \^, "for two groupoid elements", 
    IsIdenticalObj, [ IsGroupoidElement, IsGroupoidElement ], 0,
function( e1, e2 )

    local c, p, q;

    c := e2![1]; 
    p := e2![2]; 
    if ( e2![3] = p ) then  
        if ( e1![2] = p ) then 
            if ( e1![3] = p ) then 
                return ArrowNC( true, e1![1]^c, p, p ); 
            else 
                return ArrowNC( true, c^(-1)*e1![1], p, e1![3] ); 
            fi; 
        elif ( e1![3] = p ) then 
            return ArrowNC( true, e2![1]*c, e1![2], p ); 
        else 
            return e1; 
        fi; 
    else 
        q := e2![3]; 
        if ( e1![2] = p ) then 
            if ( e1![3] = p ) then
                return ArrowNC( true, e1![1]^c, q, q ); 
            elif ( e1![3] = q ) then 
                return ArrowNC( true, c^(-1)*e1![1]*c^(-1), q, p ); 
            else 
                return ArrowNC( true, c^(-1)*e1![1], q, e1![3] ); 
            fi; 
        elif ( e1![2] = q ) then 
            if ( e1![3] = q ) then 
                return ArrowNC( true, e1![1]^(c^(-1)), p, p ); 
            elif ( e1![3] = p ) then 
                return ArrowNC( true, c*e1![1]*c, p, q ); 
            else 
                return ArrowNC( true, c*e1![1], p, e1![3] ); 
            fi; 
        elif ( e1![3] = p ) then 
            return ArrowNC( true, e1![1]*c, e1![2], q ); 
        elif ( e1![3] = q ) then 
            return ArrowNC( true, e1![1]*c^(-1), e1![2], p ); 
        else 
            return e1; 
        fi;
    fi; 
end );

############################################################################# 
## 
#M  IdentityArrow
## 
InstallMethod( IdentityArrow, "for a connected groupoid and object",
    true, [ IsGroupoid and IsSinglePiece, IsObject ], 0,
function( gpd, obj ) 

    local pos, gps, id; 

    if ( HasIsGroupoidByIsomorphisms( gpd ) 
         and IsGroupoidByIsomorphisms( gpd ) ) then 
        pos := Position( gpd!.objects, obj ); 
        gps := ObjectGroups( gpd );
        id := One( gps[pos] ); 
        return Arrow( gpd, [ id, id ], obj, obj );
    else 
        return ArrowNC( true, One(gpd!.magma), obj, obj );
    fi;
end );

InstallMethod( IdentityArrow, "generic method for groupoid and object",
    true, [ IsGroupoid, IsObject ], 0,
function( gpd, obj )
    local comp;
    comp := PieceOfObject( gpd, obj );
    return ArrowNC( true, One(comp!.magma), obj, obj );
end );

############################################################################# 
## 
#M  DirectProductOp
#M  Projection 
#M  Embedding 
## 
InstallOtherMethod( DirectProductOp, "for a list of connected groupoids",
    true, [ IsList, IsGroupoid ], 0,
function( L, G )

    local Lobs, Lgps, obs, gp, prod; 

    Lobs := List( L, g -> g!.objects ); 
    obs := Cartesian( Lobs ); 
    Lgps := List( L, g -> g!.magma ); 
    gp := DirectProduct( Lgps ); 
    prod := Groupoid( gp, obs );
    SetDirectProductInfo( prod, rec( groupoids := L, 
                                     groups := Lgps, 
                                     objectlists := Lobs,
                                     first  := G, 
                                     embeddings := [], 
                                     projections := [] ) ); 
    return prod; 
end );

InstallMethod( Projection, "groupoid direct product and integer",
    [ IsGroupoid and HasDirectProductInfo, IsPosInt ], 
function( D, i )

    local info, gphom, gens, ngens, images, j, g, pg, hom;

    # check
    info := DirectProductInfo( D );
    if IsBound( info.projections[i] ) then 
        return info.projections[i];
    fi;
    # compute projection
    gphom := Projection( D!.magma, i ); 
    gens := GeneratorsOfGroupoid( D ); 
    ngens := Length( gens );
    images := ListWithIdenticalEntries( ngens, 0 ); 
    for j in [1..ngens] do 
        g := gens[j]; 
        pg := ImageElm( gphom, g![1] ); 
        images[j] := ArrowNC( true, pg, g![2][i], g![3][i] );  
    od; 
    hom := GroupoidHomomorphism( D, info.groupoids[i], gens, images ); 
    # store information
    info.projections[i] := hom;
    return hom;
end );

InstallMethod( Embedding, "groupoid direct product and integer",
    [ IsGroupoid and HasDirectProductInfo, IsPosInt ], 
function( D, i )

    local info, gphom, oblists, roots, len, ro, G, obG, nobG, imobG, 
          gens, ngens, images, j, L, g, tg, hg, eg, hom;

    # check
    info := DirectProductInfo( D );
    if IsBound( info.embeddings[i] ) then 
        return info.embeddings[i];
    fi;
    gphom := Embedding( D!.magma, i ); 
    oblists := info.objectlists; 
    roots := List( oblists, L -> L[1] ); 
    len := Length( roots );
    ro := roots[i]; 
    G := info.groupoids[i]; 
    obG := G!.objects; 
    nobG := Length( obG ); 
    imobG := ShallowCopy( obG ); 
    for j in [1..nobG] do 
        L := ShallowCopy( roots ); 
        L[i] := obG[j];
        imobG[j] := L; 
    od;
    gens := GeneratorsOfGroupoid( G ); 
    ngens := Length( gens ); 
    images := ListWithIdenticalEntries( ngens, 0 ); 
    for j in [1..ngens] do 
        g := gens[j]; 
        tg := Position( obG, g![2] );
        hg := Position( obG, g![3] );
        eg := ImageElm( gphom, g![1] );
        images[j] := ArrowNC( true, eg, imobG[tg], imobG[hg] ); 
    od; 
    hom := GroupoidHomomorphism( G, D, gens, images ); 
    # store information
    info.embeddings[i] := hom;
    return hom;
end );

############################################################################# 
## 
#M  RightActionGroupoid
## 
InstallMethod( RightActionGroupoid, "for a group", true, [ IsGroup ], 0,
function( G ) 

    local elG; 

    elG := Elements( G ); 
    return SinglePieceGroupoidWithRays( G, elG, elG ); 
end ); 

InstallMethod( RightActionGroupoid, "for a monoid", true, [ IsMonoid ], 0,
function( M )

    local elM, lenM, elG, lenG, m, e, gpd, L, x, i, K, pos, gp, comp, 
          j, obs, rays; 

    elM := Elements( M ); 
    lenM := Length( elM );
    elG := [ ]; 
    for m in M do 
        if ( m^(-1) <> fail ) then 
            Add( elG, m ); 
        fi; 
    od; 
    lenG := Length( elG );
    e := Identity( M ); 
    if ( lenG = 1 ) then 
        ## the groupoid is discrete 
        gpd := HomogeneousDiscreteGroupoid( Group(e), elM ); 
    else 
        Info( InfoGroupoids, 1, "the group of M has size ", Length( elG ) ); 
        ## construct the group component 
        gpd := SinglePieceGroupoidWithRays( Group(e), elG, elG );  
        L := ListWithIdenticalEntries( lenM, true ); 
        for x in elG do 
            L[ Position( elM, x ) ] := false; 
        od; 
        for i in [1..lenM] do 
            if L[i] then 
                ## construct the component with root elM[i]  
                x := elM[i]; 
                K := List( elG, t -> x*t ); 
                pos := Filtered( [1..lenG], j -> K[1] = K[j] ); 
                gp := Group( List( pos, j -> elG[j] ) ); 
                if ( Size( gp ) = 1 ) then 
                    comp := SinglePieceGroupoidWithRays( gp, K, elG ); 
                else 
                    obs := [ ];
                    rays := [ ];
                    for j in [1..lenG] do 
                        pos := Positions( K, K[j] ); 
                        if ( pos[1] = j ) then 
                            Add( obs, K[j] ); 
                            Add( rays, elG[j] ); 
                        fi; 
                    od; 
                    comp := SinglePieceGroupoidWithRays( gp, obs, rays ); 
                fi; 
                for j in [1..lenG] do 
                    L[ Position( elM, K[j] ) ] := false; 
                od; 
            Info( InfoGroupoids, 1, comp );
            gpd := UnionOfPieces( gpd, comp );
            fi; 
        od;
    fi; 
    SetIsGroupoidWithMonoidObjects( gpd, true );
    return gpd;
end );
