############################################################################# 
## 
#W  gpd.gi                 GAP4 package `groupoids'             Chris Wensley 
#W                                                               & Emma Moore
#Y  Copyright (C) 2000-2017, Emma Moore and Chris Wensley,  
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
    "2.  FullSubgroupoid( groupoid, list of objects );\n", 
    "3.  MaximalDiscreteSubgroupoid( groupoid );\n", 
    "4.  DiscreteSubgroupoid( groupoid, list of obs, list of subgps );\n",
    "5.  FullTrivialSubgroupoid( groupoid );\n", 
    "6.  DiscreteTrivialSubgroupoid( groupoid );\n", 
    "7.  Subgroupoid( one of the previous parameter options );" );

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
#M  SubgroupoidWithRays
#M  SubgroupoidWithRaysNC 
##
InstallMethod( SubgroupoidWithRaysNC, 
    "generic method for a connected gpd with variable object gps", true, 
    [ IsGroupoid, IsGroup, IsHomogeneousList ], 0,
function( pgpd, rgp, rays )

    local obs, rob, fam, filter, gpd, id;

    fam := IsGroupoidFamily;
    filter := IsSinglePieceRaysRep;
    gpd := Objectify( IsSinglePieceRaysType, rec () ); 
    gpd!.objects := pgpd!.objects; 
    gpd!.magma := rgp; 
    gpd!.rays := rays;
    SetIsSinglePieceDomain( gpd, true ); 
    SetLargerDirectProductGroupoid( gpd, pgpd ); 
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
        Error( "should be one ray for each object in the groupoid," ); 
    fi; 
    if not IsSubgroup( gpd!.magma, rgp ) then
        Error( "subgroupoid root group not a subgroup of the root group," );
    fi;
    if ( HasIsDirectProductWithCompleteDigraph( gpd ) 
         and IsDirectProductWithCompleteDigraph( gpd ) ) then 
        grays := List( obs, o -> One( rgp ) );
    else 
        grays := gpd!.rays; 
    fi; 
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
    par := FullSubgroupoid( anc, obs ); 
return SubgroupoidWithRays( par, gp, rays ); 
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
#M  RayElementsOfGroupoid
##
InstallMethod( RayElementsOfGroupoid, "for a connected groupoid",
    true, [ IsGroupoid and IsSinglePiece ], 0,
function( G ) 
    if ( HasIsDirectProductWithCompleteDigraphDomain( G ) 
         and IsDirectProductWithCompleteDigraphDomain( G ) ) then 
        return ListWithIdenticalEntries( 
                   Length( G!.objects ), One( G!.magma ) ); 
    else 
        return G!.rays; 
    fi;
end );

InstallMethod( RayElementsOfGroupoid, "for a groupoid", true, [ IsGroupoid ], 0,
function( G ) 
    return List( Pieces( G ), p -> RayElementsOfGroupoid( p ) ); 
end ); 

#############################################################################
##
#M  RaysOfGroupoid
##
InstallMethod( RaysOfGroupoid, "for a connected groupoid",
    true, [ IsGroupoid and IsSinglePiece ], 0,
function( gpd ) 

    local obs, nobs, rays; 

    obs := gpd!.objects; 
    nobs := Length( obs ); 
    rays := gpd!.rays; 
    return List( [1..nobs], 
               i -> Arrow( gpd, rays[i], obs[1], obs[i] ) ); 
end );

InstallMethod( RaysOfGroupoid, "for a groupoid", true, [ IsGroupoid ], 0,
function( G ) 
    return List( Pieces( G ), p -> RaysOfGroupoid( p ) ); 
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
    gens1 := List( mgens, g -> ArrowNC( true, g, o1, o1 ) ); 
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

InstallMethod( DomainWithSingleObject, "generic method for a group",
    true, [ IsGroup, IsObject ], 0,
function( gp, obj ) 

    local o; 

    if ( IsList( obj ) and ( Length(obj) = 1 ) ) then
        o := obj[1]; 
    elif IsObject( obj ) then 
        o := obj;
    else 
        Error( "usage: DomainWithSingleObject( <group>, <object> );" ); 
    fi; 
    return SinglePieceGroupoidNC( gp, [ o ] );
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
#M  \=( <g1>, <g2> )  . . . . . . . . . . . . test if two groupoids are equal
##
InstallMethod( \=, "for a connected groupoid", true, ## IsIdenticalObj,
    [ IsGroupoid and IsSinglePiece, IsGroupoid ], 
function ( g1, g2 )

    ## Print( "### method 1 for =\n" ); 
    if not IsSinglePiece( g2 ) then
        return false;
    fi; 
    if not ( IsDirectProductWithCompleteDigraph( g1 ) = 
             IsDirectProductWithCompleteDigraph( g2 ) ) then
        return false;
    fi;
    if IsDirectProductWithCompleteDigraph( g1 ) then
        return ( ( g1!.objects = g2!.objects ) and ( g1!.magma = g2!.magma ) ); 
    elif ( IsSinglePieceRaysRep( g1 ) and IsSinglePieceRaysRep( g2 ) ) then 
        return ( ( Parent( g1 ) = Parent( g2 ) ) and 
                  ( g1!.magma = g2!.magma ) and 
                  ForAll( [1..Length(g1!.rays)], 
                      j -> g1!.rays[j] * g2!.rays[j]^-1 in g1!.magma ) ); 
    else 
        Error( "method not found for g1=g2," ); 
    fi;
end );

InstallMethod( \=, "for a groupoid", true, [ IsGroupoid, IsGroupoid ], 
function ( g1, g2 )
    local c1, c2, len, obj, i, j;

    ## Print( "### method 2 for =\n" ); 
    c1 := Pieces( g1 );
    c2 := Pieces( g2 );
    len := Length( c1 );
    if ( ( len <> Length(c2) ) or ( ObjectList(g1) <> ObjectList(g2) ) ) then
        return false;
    fi;
    for i in [1..len] do
        obj := c1[i]!.objects[1];
        j := PieceNrOfObject( g2, obj );
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
InstallMethod( ObjectGroup, "generic method for groupoid and object",
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

    local pieces, hgpd; 

    pieces := List( oblist, L -> Range( IsomorphismNewObjects( gpd, L ) ) ); 
    hgpd := UnionOfPiecesOp( pieces, pieces[1] ); 
    SetIsHomogeneousDomainWithObjects( hgpd, true ); 
    SetIsSinglePieceDomain( hgpd, false ); 
    return hgpd; 
end );

InstallMethod( HomogeneousGroupoid, 
    "generic method for a connected gpd and lists of objects", true, 
    [ IsGroupoid, IsHomogeneousList ], 0,
function( gpd, oblist )

    local len, obs, ob1, j; 

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
            Info( InfoGroupoids, 1, "constituents must have disjoint object sets," );
            return fail;
        fi; 
    od; 
    for j in oblist do 
        if not ForAll( [1..Length(j)-1], i -> (j[i]<j[i+1]) ) then 
            Error( "each list in oblist should be strictly increasing," ); 
        fi;
    od; 
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
    gpd := rec( objects := obs, magma := gp ); 
    ObjectifyWithAttributes( gpd, IsHomogeneousDiscreteGroupoidType, 
        IsAssociative, true, 
        IsDiscreteDomainWithObjects, true, 
        IsHomogeneousDomainWithObjects, true, 
        IsAssociative, true, 
        IsCommutative, IsCommutative( gp ), 
        IsSinglePieceDomain, false ); 
    return gpd; 
end );


## ======================================================================= ##
##                           Groupoid Elements                             ##
## ======================================================================= ##

#############################################################################
##
#M  Arrow 
##
InstallMethod( Arrow, "generic method for a groupoid element",
    true, [ IsGroupoid, IsMultiplicativeElement, IsObject, IsObject ], 0,
function( gpd, g, i, j ) 

    local comp, obs, ok1, ok2, rays;

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
        ok2 := rays[Position(obs,i)] * g * rays[Position(obs,j)]^(-1) 
                in comp!.magma; 
    fi; 
    if not ( ok1 and ok2 ) then 
        return fail;
    else
        return ArrowNC( true, g, i, j ); 
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
##
InstallMethod( PrintObj, "for a subset of elements of a groupoid", true, 
    [ IsHomsetCosets ], 0,
function ( hc )
    
    local iter, g; 

    if ( hc![6] = "s" ) then 
        Print( "<star at ", hc![3], " with group ", hc![2], ">" );
    elif ( hc![6] = "c" ) then 
        Print( "<costar at ", hc![4], " with group ", hc![2], ">" );
    elif ( hc![6] = "h" ) then 
        Print( "<homset ", hc![3][1], " -> ", hc![4][1],
               " with group ", hc![2], ">" );
    elif ( hc![6] in [ "r", "l", "d" ] ) then 
        Print( "homset-cosets for coset" );
    else
        Print( "<object>");
    fi; 
end );

#############################################################################
##
#M  Iterator( <cset> ) . . . . . . . . . . . . . iterator for groupoid coset 
##
InstallMethod( Iterator, "for a groupoid coset", [ IsGroupoidCoset ], 
    cset -> Iterator( HomsetCosetsGroupoidCoset( cset ) ) ); 

#############################################################################
##
#M  \=( <cset> ) . . . . . . . . . . . . . . . . . . . . for groupoid cosets 
##
InstallMethod( \=, "for groupoid cosets", [IsGroupoidCoset, IsGroupoidCoset], 
function( c1, c2 ) 
    return ( ( Representative(c1) = Representative(c2) ) 
           and ( ActingDomain(c1) = ActingDomain(c2) ) 
        and ( SuperDomain(c1) = SuperDomain(c2) ) 
     and (HomsetCosetsGroupoidCoset(c1)![6]=HomsetCosetsGroupoidCoset(c2)![6])
    );
end );

#############################################################################
##
#M  Size( <homsets> ) . . . . . . . . size for star, costar, homset or coset
##
InstallMethod( Size, "for a subset of a connected groupoid", 
    [ IsHomsetCosets ], 
function( hc )
    return Size( hc![2]) * Length(hc![3]) * Length(hc![4]); 
end ); 

#############################################################################
##
#M  Iterator( <homsets> ) . . . . iterator for star, costar, homset or coset
##
InstallMethod( Iterator, "for a subset of a connected groupoid", 
    [ IsHomsetCosets ], 
function( hc )
    return IteratorByFunctions( rec( 
        IsDoneIterator := function( iter )
            return ( IsDoneIterator( iter!.groupIterator ) 
                     and ( iter!.tpos = iter!.tlen )
                     and ( iter!.hpos = iter!.hlen ) );
            end, 
        NextIterator := function( iter )
            if ( iter!.tpos = 0 ) then
                iter!.gpelt := NextIterator( iter!.groupIterator );
                iter!.tpos := 1;
                iter!.hpos := 1;
           elif ((iter!.tpos = iter!.tlen) and (iter!.hpos = iter!.hlen)) then 
                iter!.gpelt := NextIterator( iter!.groupIterator );
                iter!.tpos := 1;
                iter!.hpos := 1;
            elif ( iter!.hpos = iter!.hlen ) then
                iter!.hpos := 1;
                iter!.tpos := iter!.tpos + 1;
            else 
                iter!.hpos := iter!.hpos + 1;
            fi; 
            if ( hc![6] in [ "c", "r" ] ) then 
                return    ## GroupoidElement( iter!.fgpd!.mwo, 
                    ArrowNC( true, (hc![5][iter!.tpos])*(iter!.gpelt), 
                        iter!.tobs[iter!.tpos], iter!.hobs[iter!.hpos] );
            elif ( hc![6] in [ "s", "h" ] ) then 
                return    ## GroupoidElement( iter!.fgpd!.mwo, 
                    ArrowNC( true, (iter!.gpelt)*(hc![5][iter!.hpos]), 
                        iter!.tobs[iter!.tpos], iter!.hobs[iter!.hpos] );
            elif ( hc![6] in [ "l" ] ) then 
                return    ## GroupoidElement( iter!.fgpd!.mwo, 
                    ArrowNC( true, (iter!.gpelt)^(-1)*(hc![5][iter!.hpos]), 
                        iter!.tobs[iter!.tpos], iter!.hobs[iter!.hpos] );
            elif ( hc![6] in [ "d" ] ) then 
                Error( "double cosets not yet implemented," ); 
            fi; 
            end,
        ShallowCopy := iter -> 
            rec( groupIterator := ShallowCopy( iter!.groupIterator ), 
                 fgpd := iter!.fgpd, 
                 gpelt := iter!.gpelt,
                 tobs := iter!.tobs,
                 tlen := iter!.tlen,
                 hobs := iter!.hobs,
                 hlen := iter!.hlen,
                 tpos := iter!.tpos,
                 hpos := iter!.hpos ),
        groupIterator := Iterator( hc![2] ), 
        fgpd := hc![1], 
        gpelt := 0,
        tobs := hc![3],
        tlen := Length( hc![3] ),
        hobs := hc![4],
        hlen := Length( hc![4] ),
        tpos := 0,
        hpos := 0 ) );
end );

#############################################################################
##
#M  ObjectStarNC 
#M  ObjectStar 
##
InstallMethod( ObjectStarNC, "for a connected groupoid and an object",
    true, [ IsGroupoid and IsSinglePiece, IsObject ], 0,
function( gpd, obj )

    local gp, obs, nobs, st, fgpd, rays, pos, rpos;

    obs := gpd!.objects; 
    if ( HasIsDirectProductWithCompleteDigraph( gpd ) 
         and IsDirectProductWithCompleteDigraph( gpd ) ) then 
        gp := gpd!.magma; 
        fgpd := FamilyObj( gpd ); 
        rays := gpd!.rays; 
    else 
        fgpd := FamilyObj( Parent( gpd ) ); 
        gp := ObjectGroup( gpd, obj ); 
        rays := gpd!.rays; 
        pos := Position( obs, obj ); 
        if ( pos <> 1 ) then  ## not the root object 
            rpos := rays[pos]^(-1); 
            rays := List( [1..Length(obs)], j -> rpos*rays[j] ); 
        fi; 
    fi; 
    st := Objectify( IsHomsetCosetsType, 
                     [ fgpd, gp, [obj], obs, rays, "s" ]);
    SetIsHomsetCosets( st, true ); 
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

    local gp, obs, nobs, cst, fgpd, rays, pos, rpos;

    obs := gpd!.objects; 
    if ( HasIsDirectProductWithCompleteDigraph( gpd ) 
         and IsDirectProductWithCompleteDigraph( gpd ) ) then 
        gp := gpd!.magma; 
        fgpd := FamilyObj( gpd ); 
        rays := gpd!.rays; 
    else 
        fgpd := FamilyObj( Parent( gpd ) ); 
        gp := ObjectGroup( gpd, obj ); 
        rays := List( gpd!.rays, r -> r^(-1) ); 
        pos := Position( obs, obj ); 
        if ( pos <> 1 ) then  ## not the root object 
            rpos := rays[pos]^(-1); 
            rays := List( [1..Length(obs)], j -> rays[j]*rpos ); 
        fi; 
    fi; 
    cst := Objectify( IsHomsetCosetsType, 
                      [ fgpd, gp, obs, [obj], rays, "c" ] );
    SetIsHomsetCosets( cst, true ); 
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

    local gp, obs, fgpd, rays, gen, hs, p1, p2;
    
    obs := gpd!.objects; 
    if ( HasIsDirectProductWithCompleteDigraph( gpd ) 
         and IsDirectProductWithCompleteDigraph( gpd ) ) then 
        gp := gpd!.magma;
        fgpd := FamilyObj( gpd ); 
        gen := [ One(gp) ]; 
    else 
        fgpd := FamilyObj( Parent( gpd ) ); 
        gp := ObjectGroup( gpd, o1 ); 
        p1 := Position( obs, o1 ); 
        p2 := Position( obs, o2 );
        gen := [ gpd!.rays[p1]^(-1) * gpd!.rays[p2] ]; 
    fi; 
    hs := Objectify( IsHomsetCosetsType, 
                     [ fgpd, gp, [o1], [o2], gen, "h" ] );
    SetIsHomsetCosets( hs, true ); 
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
InstallMethod( SubgroupoidBySubgroup,
    "generic method for direct prod with complete graph and subgroup", true,
    [ IsGroupoid and IsDirectProductWithCompleteDigraph, IsGroup ], 0,
function( gpd, sgp ) 

    local sub, gp; 

    gp := gpd!.magma; 
    if not IsSubgroup( gp, sgp ) then 
        Error( "sgp is not a subgroup of gpd!.magma," ); 
    fi; 
    sub := SinglePieceGroupoid( sgp, gpd!.objects );
    SetParentAttr( sub, gpd );
    return sub; 
end ); 

#############################################################################
##
#M  SubgroupoidByPieces
##
InstallMethod( SubgroupoidByPieces,
    "generic method for a groupoid and a list of pieces", true,
    [ IsGroupoid, IsList ], 0, 
function( gpd, pieces )

    local pieceU, U, c, piece;

    if IsList( pieces[1] ) then 
        pieceU := List( pieces, c -> SinglePieceGroupoid( c[1], c[2] ) ); 
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
        SetParentAttr( c, FullSubgroupoid( piece, ObjectList(c) ) );
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

    local pieceU, U, len, o, pieceG, obsg, C, i, gpo;

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
            Error( "subgroupoid object not in groupoid," );
        fi;
        C := PieceOfObject( G, o ); 
        gpo := ObjectGroup( C, o ); 
        if not IsSubgroup( gpo, gps[i] ) then
            Error( "not a subgroup of object group," );
        fi;
        pieceU[i] := [ gps[i], [o] ];
    od; 
    if ForAll( gps, g -> ( g = gps[1] ) ) then 
        Info( InfoGroupoids, 2, 
              "all groups equal, so using HomogeneousDiscreteGroupoid" ); 
        U := HomogeneousDiscreteGroupoid( gps[1], obs ); 
    else 
        U := SubgroupoidByPieces( G, pieceU ); 
    fi;
    SetIsDiscreteDomainWithObjects( U, true );
    SetParentAttr( U, G );
    return U;
end );

#############################################################################
##
#M  MaximalDiscreteSubgroupoid
##
InstallMethod( MaximalDiscreteSubgroupoid,
    "generic method for a groupoid", true, [ IsGroupoid ], 0,
function( gpd )

    local obs, len, gps, i, piece;

    obs := ObjectList( gpd );
    len := Length( obs );
    gps := ListWithIdenticalEntries( len, 0 );
    for i in [1..len] do
        piece := PieceOfObject( gpd, obs[i] );
        gps[i] := ObjectGroup( piece, obs[i] ); 
    od;
    return DiscreteSubgroupoid( gpd, gps, obs );
end );

#############################################################################
##
#M  FullSubgroupoid
#M  FullTrivialSubgroupoid
#M  DiscreteTrivialSubgroupoid
##
InstallMethod( FullSubgroupoid, "for a connected groupoid and set of objects", 
    true, [ IsGroupoid and IsSinglePiece, IsHomogeneousList ], 0,
function( G, sobs )

    local sgpd, rgp, rayG, pG, spG, pos, rays, r1; 

    if ( HasIsDirectProductWithCompleteDigraph( G ) 
         and IsDirectProductWithCompleteDigraph( G ) ) then 
        if not HasIsSSortedList( sobs ) then 
            Sort( sobs ); 
        fi; 
        sgpd :=  SinglePieceGroupoidNC( G!.magma, sobs ); 
        SetParentAttr( sgpd, G ); 
    else 
        if HasLargerDirectProductGroupoid( G ) then 
            pG := LargerDirectProductGroupoid( G ); 
        else 
            Print( "#I should not reach this point!\n" ); 
            pG := Parent( G ); 
        fi; 
        spG := FullSubgroupoid( pG, sobs ); 
        rgp := ObjectGroup( G, sobs[1] );  
        rayG := G!.rays; 
        pos := List( sobs, j -> Position( G!.objects, j ) ); 
        r1 := rayG[ pos[1] ]^(-1); 
        rays := List( pos, j -> r1*rayG[j] ); 
        sgpd := SubgroupoidWithRays( spG, rgp, rays );
        SetParentAttr( sgpd, spG ); 
    fi; 
    return sgpd; 
end );

InstallMethod( FullSubgroupoid, "for a groupoid and set of objects", true,
    [ IsGroupoid, IsHomogeneousList ], 0,
function( G, sobs ) 

    local c1, c2, len, j, c, obc, sobc;

    c1 := Pieces( G );
    len := Length( c1 );
    c2 := [ ];
    for j in [1..len] do
        c := c1[j];
        obc := c!.objects;
        sobc := Intersection( sobs, obc );
        if ( sobc <> [ ] ) then
            Add( c2, [ c!.magma, sobc ] );
        fi;
    od;
    return SubgroupoidByPieces( G, c2 );
end );

InstallMethod( FullSubgroupoid, "for a homogeneous discrete groupoid", true,
    [ IsHomogeneousDiscreteGroupoid, IsHomogeneousList ], 0,
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
#M  RightTransversal 
#M  RightCoset 
#M  LeftCoset
#M  DoubleCoset
##
#? (24/09/08)  decided to include the larger groupoid - a mistake ?? 
#? (24/09/08)  should split this into a non-NC and an NC operation ?? 
#? (03/10/08)  now allowing U not to be wide in G (see RightCosets). 

InstallOtherMethod( RightCoset, "for groupoid, subgroupoid and element", 
    true,  ##  IsCollsElms ?? 
    [ IsGroupoid, IsGroupoid, IsMultiplicativeElementWithObjects ],
    0, 
function( gpd, sgpd, e ) 

    local G, U, fam, r, hc; 

    if IsSinglePiece( gpd ) then 
        G := gpd; 
    else
        Info( InfoGroupoids, 2, "comment: gpd not a single piece!" ); 
        G := PieceOfObject( gpd, e![2] ); 
    fi; 
    if IsSinglePiece( sgpd ) then 
        U := sgpd; 
    else
        Info( InfoGroupoids, 2, "comment: sgpd not a single piece!" ); 
        U := PieceOfObject( sgpd, e![2] ); 
    fi; 
    if not IsSubdomainWithObjects( G, U ) then 
        Error( "U not a subgroupoid of G," ); 
    fi; 
    fam := FamilyObj( One( G!.magma ) ); 
    if not IsBound(fam!.rightGroupoidCosetsDefaultSizeType) then
        fam!.rightGroupoidCosetsDefaultSizeType 
            := NewType( fam, IsRightCosetDefaultRep and HasActingDomain 
               and HasFunctionAction and HasRepresentative and HasSize );
    fi;
    #?  (16/05/11)  r thinks it is groupoid, so PrintObj fails ?? 
    #?              probably using the wrong family construction ??  
    r := rec(); 
    #?  seems not possible to use IsHomsetCosetsType here 
    ObjectifyWithAttributes( r, fam!.rightGroupoidCosetsDefaultSizeType, 
        IsGroupoidCoset, true, 
        SuperDomain, G, 
        ActingDomain, U, 
#?        FunctionAction, OnLeftInverse,           (18/09/08) incorrect! 
        Size, Size( ObjectCostar( U, e![2] ) ), 
        Representative, e ); 
    hc := HomsetCosetsGroupoidCoset( r ); 
    return r; 
end ); 

InstallMethod( LeftCoset, "for groupoid, subgroupoid and element", 
    true,  ##  IsCollsElms ?? 
    [ IsGroupoid, IsGroupoid, IsGroupoidElement ],
    0, 
function( gpd, sgpd, e ) 

    local G, U, fam, l, hc; 

    if IsSinglePiece( gpd ) then 
        G := gpd; 
    else
        Info( InfoGroupoids, 2, "comment: gpd not a single piece!" ); 
        G := PieceOfObject( gpd, e![2] ); 
    fi; 
    if IsSinglePiece( sgpd ) then 
        U := sgpd; 
    else
        Info( InfoGroupoids, 2, "comment: sgpd not a single piece!" ); 
        U := PieceOfObject( sgpd, e![2] ); 
    fi; 
    if not IsSubdomainWithObjects( G, U ) then 
        Error( "U not a subgroupoid of G," ); 
    fi; 
    fam := FamilyObj( G );
    if not IsBound(fam!.leftGroupoidCosetsDefaultSizeType) then
        fam!.leftGroupoidCosetsDefaultSizeType 
            := NewType( fam, IsLeftCosetWithObjectsDefaultRep 
                   and HasActingDomain and HasFunctionAction 
                   and HasRepresentative and HasSize );
    fi;
    l := rec();
    ObjectifyWithAttributes( l, fam!.leftGroupoidCosetsDefaultSizeType, 
        IsGroupoidCoset, true, 
        SuperDomain, G, 
        ActingDomain, U, 
#?        FunctionAction, OnLeftInverse,           (18/09/08) incorrect!
        Size, Size( ObjectStar( U, e![3] ) ),   
        Representative, e ); 
    hc := HomsetCosetsGroupoidCoset( l ); 
    return l;
end ); 

InstallMethod( PrintObj, "RightCoset", true, 
    [ IsGroupoidCoset and IsRightCosetDefaultRep ], 0,
function( l )
  Print( "RightCoset(", ActingDomain(l), ",", Representative(l), ")" );
end); 

InstallMethod( ViewObj, "RightCoset", true, 
    [ IsGroupoidCoset and IsRightCosetDefaultRep ], 0,
function( l )
  Print( "RightCoset(", ActingDomain(l), ",", Representative(l), ")" );
end); 

InstallMethod( PrintObj, "LeftCoset", true, 
    [ IsGroupoidCoset and IsLeftCosetWithObjectsDefaultRep ], 0,
function( l )
  Print( "LeftCoset(", Representative(l), ",", ActingDomain(l), ")" );
end); 

InstallMethod( ViewObj, "LeftCoset", true, 
    [ IsGroupoidCoset and IsLeftCosetWithObjectsDefaultRep ], 0,
function( l )
  Print( "LeftCoset(", Representative(l), ",", ActingDomain(l), ")" );
end); 

InstallMethod( HomsetCosetsGroupoidCoset, "for right coset of groupoid",
    true, [ IsGroupoidCoset ], 0,
function( cset )

    local U, e, G, obs, ogp, o1, o2, gpcset, fgpd, rays, pos, rpos, 
          hc, isrt, C; 

    e := Representative( cset ); 
    G := SuperDomain( cset ); 
    U := ActingDomain( cset ); 
    if not e in G then 
        Error( "element e not in groupoid G," ); 
    fi; 
    isrt := IsRightCosetDefaultRep( cset ); 
    o1 := e![2];
    o2 := e![3]; 
    if isrt then 
        ogp := ObjectGroup( U, o1 ); 
    else 
        ogp := ObjectGroup( U, o2 ); 
    fi; 
    if ( HasIsSinglePiece(U) and IsSinglePiece(U) ) then 
        C := U; 
    elif isrt then 
        C := PieceOfObject( U, o1 ); 
    else 
        C := PieceOfObject( U, o2 ); 
    fi; 
    obs := C!.objects;
    if ( HasIsDirectProductWithCompleteDigraph( C ) 
         and IsDirectProductWithCompleteDigraph( C ) ) then 
        Info( InfoGroupoids, 2, "Option 1 in HomsetCosetsGroupoidCoset" ); 
        fgpd := FamilyObj( C ); 
        rays := C!.rays; 
    else 
        Info( InfoGroupoids, 2, "Option 2 in HomsetCosetsGroupoidCoset" ); 
        fgpd := FamilyObj( Parent( C ) );  
        rays := List( C!.rays, r -> r^(-1) ); 
        if isrt then 
            pos := Position( obs, o1 ); 
        else 
            pos := Position( obs, o2 ); 
        fi; 
        if ( pos <> 1 ) then   ## not the root object 
            rpos := rays[pos]^(-1); 
            if isrt then 
                rays := List( [1..Length(obs)], j -> rays[j]*rpos ); 
            else
                rays := List( [1..Length(obs)], j -> rpos*rays[j] ); 
            fi; 
        fi; 
    fi; 
    if isrt then 
        gpcset := RightCoset( ogp, e![1] ); 
        hc := Objectify( IsHomsetCosetsType, 
                         [ fgpd, gpcset, obs, [o2], rays, "r" ] );
    elif IsLeftCosetWithObjectsDefaultRep( cset ) then 
        gpcset := RightCoset( ogp, e![1]^(-1) ); 
        hc := Objectify( IsHomsetCosetsType, 
                         [ fgpd, gpcset, [o1], obs, rays, "l" ] ); 
    fi; 
    SetIsHomsetCosets( hc, true ); 
    return hc;
end);

#############################################################################
########  Why not `IsIdenticalObj' in the following declarations ??? ########

############################################################################# 
## 
#M  RightCosetRepresentatives
## 
InstallMethod( RightCosetRepresentatives, "generic method for a subgroupoid",
    true, [ IsGroupoid and IsSinglePiece, IsGroupoid ], 0,
function( G, U )

    local gg, obG, nobG, reps, c, o1, gc, rc, nrc, L, j, b, g, o;

    if not IsWideSubgroupoid( G, U ) then
        Error( "U not a wide subgroupoid of G," );
    fi;
    gg := G!.magma;
    obG := G!.objects;
    nobG := Length( obG );
    reps := [ ];
    for c in Pieces( U ) do
        o1 := c!.objects[1];
        gc := c!.magma;
        rc := RightCosets( gg, gc );
        nrc := Length( rc );
        L := ListWithIdenticalEntries( nrc*nobG, 0 );
        j := 0;
        for b in rc do
            g := Representative(b);
            for o in obG do
                j := j+1; 
                L[j] := ArrowNC( true, g, o1, o );
            od;
        od;
        Append( reps, L );
    od;
    return reps;
end );    

InstallMethod( RightCosetRepresentatives, "generic method for a subgroupoid",
    true, [ IsGroupoid, IsGroupoid ], 0,
function( G, U )

    local pos, cG, cU, ncU, reps, i, filt, piece, m, j, subU;

    if not IsWideSubgroupoid( G, U ) then
        Error( "U not a wide subgroupoid of G," );
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

    local gg, obG, nobG, reps, c, o1, gc, rc, nrc, L, j, b, g, o;

    if not IsWideSubgroupoid( G, U ) then
        Error( "U not a wide subgroupoid of G," );
    fi;
    gg := G!.magma;
    obG := G!.objects;
    nobG := Length( obG );
    reps := [ ];
    for c in Pieces( U ) do
        o1 := c!.objects[1];
        gc := c!.magma;
        rc := RightCosets( gg, gc );
        nrc := Length( rc );
        L := ListWithIdenticalEntries( nrc*nobG, 0 );
        j := 0;
        for b in rc do
            g := Representative(b)^(-1);
            for o in obG do
                j := j+1;
                L[j] := ArrowNC( true, g, o, o1 );
            od;
        od;
        Append( reps, L );
    od;
    return reps;
end );    

InstallMethod( LeftCosetRepresentatives, "generic method for a subgroupoid",
    true, [ IsGroupoid, IsGroupoid ], 0,
function( G, U )

    local pos, cG, cU, ncU, reps, i, filt, piece, m, j, subU;

    if not IsWideSubgroupoid( G, U ) then
        Error( "U not a wide subgroupoid of G," );
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

    local gg, obG, nobG, reps, c, o1, gc, rc, nrc, L, j, b, g;

    if not IsWideSubgroupoid( G, U ) then
        Error( "U not a wide subgroupoid of G," );
    fi;
    gg := G!.magma;
    obG := G!.objects;
    if not ( o in obG ) then
        Error( "chosen object not in the groupoid," );
    fi;
    reps := [ ];
    for c in Pieces( U ) do
        o1 := c!.objects[1];
        gc := c!.magma;
        rc := RightCosets( gg, gc );
        nrc := Length( rc );
        L := ListWithIdenticalEntries( nrc, 0 );
        j := 0;
        for b in rc do
            g := Representative(b)^(-1);
            j := j+1;
            L[j] := ArrowNC( true, g, o, o1 );
        od;
        Append( reps, L );
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
##  RightCosets(F,U) where F is the FullSubgroupoid of G on Objects(U). 
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
    
InstallMethod( LeftCosetsNC, "for groupoids", true, 
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
        cosets[i] := DoubleCoset( U, e, V );
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

##  This operator now (05/03/08) follows the definitions in preprint 07.10

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
#M  \^( <gpd>, <elt> )
##
##  InstallOtherMethod( \^, "generic method for groupoid and element",
##      IsCollsElms, [ IsGroupoid, IsGroupoidElement ], ConjugateGroupoid );

############################################################################# 
## 
#M  IdentityArrow
## 
InstallMethod( IdentityArrow, "for a connected groupoid and object",
    true, [ IsGroupoid and IsSinglePiece, IsObject ], 0,
function( gpd, obj )
    return ArrowNC( true, One(gpd!.magma), obj, obj );
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

##############################################################################
##
#E  gpd.gi . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##
