############################################################################
##
#W  gpdgraph.gi             GAP4 package `groupoids'           Chris Wensley
#W                                                              & Emma Moore
##
##  This file contains methods for FpWeightedDigraphs of groupoids, 
##  and normal forms for GraphOfGroupoidWords.

#############################################################################
##
#M  GraphOfGroupoidsNC                                               
#M  GraphOfGroupoids                                             
##
InstallMethod( GraphOfGroupoidsNC, "generic method for digraph of groupoids",
    true, [ IsFpWeightedDigraph, IsList, IsList, IsList ], 0,
function( dig, gpds, subgpds, isos )

    local fam, filter, gg;
     
    fam := GraphOfGroupoidsFamily; 
    filter := IsGraphOfGroupoidsRep; 
    gg := rec(); 
    ObjectifyWithAttributes( gg, GraphOfGroupoidsType, 
        DigraphOfGraphOfGroupoids, dig, 
        GroupoidsOfGraphOfGroupoids, gpds, 
        SubgroupoidsOfGraphOfGroupoids, subgpds, 
        IsomorphismsOfGraphOfGroupoids, isos );
    if ForAll( gpds, IsPermGroupoid ) then
        SetIsGraphOfPermGroupoids( gg, true );
    elif ForAll( gpds, IsFpGroupoid ) then
        SetIsGraphOfFpGroupoids( gg, true );
    fi;
    return gg; 
end );

InstallMethod( GraphOfGroupoids, "generic method for a digraph of  groupoids",
    true, [ IsFpWeightedDigraph, IsList, IsList, IsList ], 0,
function( dig, gpds, subgpds, isos )

    local g, ob1, nob1, m, i, j, v, a, lenV, lena, tgtL, pos, inv, ainvpos;

    v := dig!.vertices;
    a := dig!.arcs; 
    lenV := Length(v); 
    lena := Length(a);
    # checking that list sizes are compatible
    if ((lenV <> Length(gpds)) or (lena <> Length(subgpds)) or 
           (lena <> Length(isos))) then
        Error( "list sizes are not compatible for assignments" );
    fi; 
    # checking that we have groupoids
    # lenV and length of groupoids are equal 
    ob1 := ObjectList( gpds[1] );
    nob1 := Length( ob1 ); 
    for i in [1..lenV] do
        if not IsGroupoid( gpds[i] ) then
            Error( "groupoids are needed");
        fi;
        if not ( Length( ObjectList( gpds[i] ) ) = nob1 ) then
            Error( "groupoids must have same number of objects");
        fi;
#        if not ( Objects( gpds[i] ) = ob1 ) then
#            Error( "groupoids fail to have the same objects" );
#        fi;
    od;
    # checking that subgroupoids are groupoids
    for i in [1..lena] do
        if not IsGroupoid( subgpds[i] ) then
            Error( "not a subgroupoid");
        fi;
    od;
    # checking that subgroupoids are subgroupoids of the relevant groupoid    
    for i in [1..lena] do
        pos := Position( v, a[i][2] ); 
        if not IsSubgroupoid( gpds[pos], subgpds[i] ) then
            Error( "subgroupoids are not of correct groupoids");
        fi;
    od;
    # checking that isomorphisms are isos and form correct groupoids.
    inv := InvolutoryArcs( dig ); 
    for i in [1..lena] do
        m := isos[i];
#        if not ( Objects( Source(m) ) = ObjectImages(m) ) then
#            Error( "isos not constant on objects" );
#        fi;
#        ainvpos := Position( a, a[inv[i]] );
        if not( isos[inv[i]] = InverseGeneralMapping(m) ) then
            Error( "isos are not in inverse pairs");
        fi;
    od; 
    return GraphOfGroupoidsNC( dig, gpds, subgpds, isos );
end );

#############################################################################
##
#M  String, ViewString, PrintString, ViewObj, PrintObj 
##  . . . . . . . . . . . . . . . . . . . . . . . . for a graph of groupoids 
##
InstallMethod( String, "for a graph of groupoids", true, 
    [ IsGraphOfGroupoids ], 0, 
function( gg ) 
    return( STRINGIFY( "graph of groupoids" ) ); 
end );

InstallMethod( ViewString, "for a graph of groupoids", true, 
    [ IsGraphOfGroupoids ], 0, String ); 

InstallMethod( PrintString, "for a graph of groupoids", true, 
    [ IsGraphOfGroupoids ], 0, String ); 

InstallMethod( ViewObj, "for a graph of groupoids", true, 
    [ IsGraphOfGroupoids ], 0, PrintObj ); 

InstallMethod( PrintObj, "for a graph of groupoids", [ IsGraphOfGroupoids ],
function( gg )
    
    local dig;

    dig := DigraphOfGraphOfGroupoids( gg );
    Print( "Graph of Groupoids: " );
    Print( Length( dig!.vertices ), " vertices; " );
    Print( Length( dig!.arcs ), " arcs; " );
    ## Print( Length(GroupoidVertices(dig)), " vertices; " );
    ## Print( Length(GroupoidArcs(dig)), " arcs; " );
    Print( "groupoids ", GroupoidsOfGraphOfGroupoids( gg ) );
end );

InstallMethod( ViewObj, "for graph of groupoids", [ IsGraphOfGroupoids ],
function( gg )
    
    local dig; 

    dig := DigraphOfGraphOfGroupoids( gg );
    Print( "Graph of Groupoids: " );
    Print( Length( dig!.vertices ), " vertices; " );
    Print( Length( dig!.arcs ), " arcs; " );
    ## Print( Length(GroupoidVertices(dig)), " vertices; " );
    ## Print( Length(GroupoidArcs(dig)), " arcs; " );
    Print( "groupoids ", GroupoidsOfGraphOfGroupoids( gg ) );
end );

##############################################################################
##
#M  Display( <gg> ) . . . . . . . . . . . . . . . . view a graph of groupoids
##
InstallMethod( Display, "for a graph of groupoids", [ IsGraphOfGroupoids ],
function( gg )
    
    local g, dig; 

    dig := DigraphOfGraphOfGroupoids( gg );
    Print( "Graph of Groupoids with :- \n" );
    Print( "    vertices: ", dig!.vertices, "\n" );
    Print( "        arcs: ", dig!.arcs, "\n" );
    ## Print( "    vertices: ", GroupoidVertices( dig ), "\n" );
    ## Print( "        arcs: ", GroupoidArcs( dig ), "\n" );
    Print( "   groupoids: \n" );
    for g in GroupoidsOfGraphOfGroupoids( gg ) do
        Display( g );
    od;
    Print( "subgroupoids: " );
    for g in SubgroupoidsOfGraphOfGroupoids( gg ) do
        Display( g );
    od;
    Print( "isomorphisms: ", IsomorphismsOfGraphOfGroupoids( gg ), "\n" );
end );

##############################################################################
##
#M  IsGraphOfPermGroupoids( <gg> ) . . . . . . . . . for a graph of groupoids
#M  IsGraphOfFpGroupoids( <gg> ) . . . . . . . . . . for a graph of groupoids
#M  IsGraphOfPcGroupoids( <gg> ) . . . . . . . . . . for a graph of groupoids
##
InstallMethod( IsGraphOfPermGroupoids, "generic method", [ IsGraphOfGroupoids ],
function( gg )
    return ForAll( GroupoidsOfGraphOfGroupoids( gg ), IsPermGroupoid );
end );

InstallMethod( IsGraphOfFpGroupoids, "generic method", [ IsGraphOfGroupoids ],
function( gg )
    return ForAll( GroupoidsOfGraphOfGroupoids( gg ), IsFpGroupoid );
end );

InstallMethod( IsGraphOfPcGroupoids, "generic method", [ IsGraphOfGroupoids ],
function( gg )
    return ForAll( GroupoidsOfGraphOfGroupoids( gg ), IsPcGroupoid );
end );

#############################################################################
##
#M  NormalFormKBRWS                                             
##
InstallOtherMethod( NormalFormKBRWS, "generic method for normal form",
    true, [ IsFpGroupoid, IsObject ], 0,
function( gpd, w0 )

    local comp, ogp, id, iso, inviso, smg, rwsmg, smggen, fsmg,
          iw, uiw, ruw, fam1, riw, rw;

    if not w0 in gpd then
        Error( "word not in the groupoid" );
    fi;
    comp := PieceOfObject( gpd, w0![3] );
    ogp := comp!.magma;
    id := One( ogp );
    iso := IsomorphismFpSemigroup( ogp );
    inviso := InverseGeneralMapping( iso );
    smg := Range( iso );
    rwsmg := KnuthBendixRewritingSystem( smg );
    MakeConfluent( rwsmg );  ### this should not be necessary here !! ###
    smggen := GeneratorsOfSemigroup( smg );
    fsmg := FreeSemigroupOfKnuthBendixRewritingSystem( rwsmg );
    iw := ImageElm( iso, w0![2] );
    uiw := UnderlyingElement( iw );
    ruw := ReducedForm( rwsmg, uiw );
    fam1 := FamilyObj( smggen[1] );
    riw := ElementOfFpSemigroup( fam1, ruw );
    rw := ImageElm( inviso, riw );
    return GroupoidElement( gpd, rw, w0![3], w0![4] );
end);

#############################################################################
##
#M  RightTransversalsOfGraphOfGroupoids
##
InstallMethod( RightTransversalsOfGraphOfGroupoids, 
    "generic method for a groupoid graph", true, [ IsGraphOfGroupoids ], 0,
function( gg )

    local gpds, subs, dig, verts, arcs, na, reps, k, a, p, G, U;

    gpds := GroupoidsOfGraphOfGroupoids( gg );
    subs := SubgroupoidsOfGraphOfGroupoids( gg );
    dig := DigraphOfGraphOfGroupoids( gg );
    verts := dig!.vertices;
    arcs := dig!.arcs;
    ## verts := GroupoidVertices( dig );
    ## arcs := GroupoidArcs( dig );
    na := Length( arcs );
    reps := ListWithIdenticalEntries( na, 0 );
    for k in [1..na] do
        a := arcs[k];
        p := Position( verts, a![3] );
        G := gpds[p];
        U := subs[k];
        reps[k] := RightCosetRepresentatives( G, U );
    od;
    return reps;
end );

#############################################################################
##
#M  LeftTransversalsOfGraphOfGroupoids
##
InstallMethod( LeftTransversalsOfGraphOfGroupoids, 
    "generic method for a groupoid graph", true, [ IsGraphOfGroupoids ], 0,
function( gg )

    local gpds, subs, dig, verts, arcs, na, reps, k, a, p, G, obG, U;

    gpds := GroupoidsOfGraphOfGroupoids( gg );
    subs := SubgroupoidsOfGraphOfGroupoids( gg );
    dig := DigraphOfGraphOfGroupoids( gg );
    verts := dig!.vertices;
    arcs := dig!.arcs;
    na := Length( arcs );
    reps := ListWithIdenticalEntries( na, 0 );
    for k in [1..na] do
        a := arcs[k];
        p := Position( verts, a![2] );
        G := gpds[p];
        obG := ObjectList( G );
        U := subs[k];
        reps[k] := List( obG, 
            o -> LeftCosetRepresentativesFromObject( G, U, o ) );
    od;
    return reps;
end);

## ------------------------------------------------------------------------##
##                      Graph of Groupoids Words                           ##
## ------------------------------------------------------------------------##
 
#############################################################################
##
#M  GraphOfGroupoidsWordNC                                               
#M  GraphOfGroupoidsWord 
##
InstallMethod( GraphOfGroupoidsWordNC, "generic method for a word",
    true, [ IsGraphOfGroupoids, IsInt, IsList ], 0,
function( gg, tv, wL )

    local fam, filter, ggword;

    fam := FamilyObj( [ gg, wL] );
    filter := IsGraphOfGroupoidsWordRep;
    ggword := rec(); 
    ObjectifyWithAttributes( ggword, IsGraphOfGroupoidsWordType, 
        GraphOfGroupoidsOfWord, gg, 
        TailOfGraphOfGroupsWord, tv, 
        WordOfGraphOfGroupoidsWord, wL, 
        IsGraphOfGroupoidsWord, true );
    if ( Length( wL ) = 1 ) then
        SetHeadOfGraphOfGroupsWord( ggword, tv );
    fi;
    return ggword; 
end );

InstallMethod( GraphOfGroupoidsWord, "for word in graph of groupoids",
    true, [ IsGraphOfGroupoids, IsInt, IsList ], 0,
function( gg, tv, wL )

    local gpds, dig, arcs, enum, vdig, n, i, j, g, cg, v, posv, e, w;

    gpds := GroupoidsOfGraphOfGroupoids( gg );
    dig := DigraphOfGraphOfGroupoids( gg );
    vdig := dig!.vertices;
    arcs := dig!.arcs;
    enum := Length( arcs );
    v := tv;
    posv := Position( vdig, v );
    g := gpds[ posv ];
    w := wL[1];
    cg := PieceOfObject( g, w![3] );
    if not w in cg then
        Error( "first groupoid element not in tail groupoid" );
    fi;
    j := 1;
    n := ( Length( wL ) - 1 )/2;
    for i in [1..n] do
        e := wL[j+1];
        if ( e > enum ) then
            Error( "entry ", j+1, " in wL not an arc" );
        else
            e := arcs[e];
        fi;
        v := e[3];
        posv := Position( vdig, v );
        g := gpds[ posv ];
        j := j+2;
        w := wL[j];
        cg := PieceOfObject( g, w![3] );
        if not w in cg then
            Error( "entry ", j, " not in groupoid at vertex ", v );
        fi;
    od;    
    Info( InfoGroupoids, 3, "wL = ", wL );
    return GraphOfGroupoidsWordNC( gg, tv, wL );
end);

#############################################################################
##
#M  String, ViewString, PrintString, ViewObj, PrintObj 
##  . . . . . . . . . . . . . . . . . . . . . for a graph of groupoids word 
##
InstallMethod( String, "for a graph of groupoids word", true, 
    [ IsGraphOfGroupoidsWord ], 0, 
function( ggword ) 
    return( STRINGIFY( "graph of groupoids word" ) ); 
end );

InstallMethod( ViewString, "for a graph of groupoids word", true, 
    [ IsGraphOfGroupoidsWord ], 0, String ); 

InstallMethod( PrintString, "for a graph of groupoids word", true, 
    [ IsGraphOfGroupoidsWord ], 0, String ); 

InstallMethod( ViewObj, "for a graph of groupoids word", 
    [ IsGraphOfGroupoidsWord ],
function( ggword )
    local w, i, gg, arcs;

    gg := GraphOfGroupoidsOfWord( ggword );
    arcs := DigraphOfGraphOfGroupoids( gg )!.arcs;
    ## arcs := GroupoidArcs( DigraphOfGraphOfGroupoids( gg ) );
    w := WordOfGraphOfGroupoidsWord( ggword );
    Print( "(", TailOfGraphOfGroupsWord( ggword ), ")", w[1] );
    i := 1;
    while ( i < Length(w) ) do
        i := i+2;
        Print( ".", arcs[w[i-1]][1], ".", w[i] );
    od;
    Print( "(", HeadOfGraphOfGroupsWord( ggword ), ")" );
end );

InstallMethod( PrintObj, "for a graph of groupoids word", 
    [ IsGraphOfGroupoidsWord ],
function( ggword ) 

    local w, i, gg, arcs;

    gg := GraphOfGroupoidsOfWord( ggword );
    arcs := DigraphOfGraphOfGroupoids( gg )!.arcs;
    ## arcs := GroupoidArcs( DigraphOfGraphOfGroupoids( gg ) );
    w := WordOfGraphOfGroupoidsWord( ggword );
    Print( "(", TailOfGraphOfGroupsWord( ggword ), ")", w[1] );
    i := 1;
    while ( i < Length(w) ) do
        i := i+2;
        Print( ".", arcs[w[i-1]][1], ".", w[i] );
    od;
    Print( "(", HeadOfGraphOfGroupsWord( ggword ), ")" );
end );

#############################################################################
##
#M  HeadOfGraphOfGroupsWord                                             
##
InstallOtherMethod( HeadOfGraphOfGroupsWord, "generic method for a graph of groupoids word",
    true, [ IsGraphOfGroupoidsWordRep ], 0,
function( ggword )

    local w, gg, e, ie;

    w := WordOfGraphOfGroupoidsWord( ggword ); 
    gg := GraphOfGroupoidsOfWord( ggword );
    e := w[Length(w)-1];
    return DigraphOfGraphOfGroupoids( gg )!.arcs[e][3];
    ## return GroupoidArcs( DigraphOfGraphOfGroupoids( gg ) )[e][3];
end );

#############################################################################
##
#M  ReducedGraphOfGroupoidsWord 
##
InstallMethod( ReducedGraphOfGroupoidsWord, "for word in graph of groupoids",
    true, [ IsGraphOfGroupoidsWordRep ], 0,
function( ggword )

    local w, tw, hw, gg, gpds, sgpds, dig, adig, vdig, lw, len, k, k2, he,
          tsp, word, tran, ltrans, pos, a, g, h, found, i, nwit, tword, 
          te, obg, nob, ptword, itword, ch, isos, im, sub, u, v, gu, gv, e, 
          ie, isoe, part, cw, pw, fw, iw, isow, ng, rw, lenred, isfp, isid;

    if ( HasIsReducedGraphOfGroupoidsWord( ggword )
         and IsReducedGraphOfGroupoidsWord( ggword ) ) then
        return ggword;
    fi;
    w := ShallowCopy( WordOfGraphOfGroupoidsWord( ggword ) );
    tw := TailOfGraphOfGroupsWord( ggword );
    hw := HeadOfGraphOfGroupsWord( ggword );
    lw := Length( w );
    len := (lw-1)/2;
    gg := GraphOfGroupoidsOfWord( ggword );
    gpds := GroupoidsOfGraphOfGroupoids( gg );
    sgpds := SubgroupoidsOfGraphOfGroupoids( gg );
    isos := IsomorphismsOfGraphOfGroupoids( gg );
    isfp := IsGraphOfFpGroupoids( gg );
    dig := DigraphOfGraphOfGroupoids( gg );
    vdig := dig!.vertices;
    adig := dig!.arcs;
    ltrans := LeftTransversalsOfGraphOfGroupoids( gg );
    if ( len = 0 ) then
        if isfp then
            ng := NormalFormKBRWS( gpds[tw], w[1] ); 
        else
            ng := w[1];
        fi;
        return GraphOfGroupoidsWordNC( gg, tw, ng );
    fi;
    k := 1;
    v := tw;
    while ( k <= len ) do
        k2 := k+k;
        ## reduce the subword  w{[k2-1..k2+1]}
        e := w[k2];
        he := Position( vdig, adig[e][3] );
        #### factor groupoid element as pair [ transversal, subgpd elt ] ####
        word := w[k2-1];
        Info( InfoGroupoids, 3, "[word,e] = ", word, ", ", e );
        a := adig[e];
        te := Position( vdig, a[2] );
        g := gpds[te];
        u := sgpds[te];
        obg := ObjectList( g );
        nob := Length( obg );
        tword := word![3];
        ptword := Position( obg, tword );
        h := sgpds[e];
        ch := PieceOfObject( h, word![4] );
        tran := ltrans[e][ptword];
        Info( InfoGroupoids, 3, "tran = ", tran );
        Info( InfoGroupoids, 3, "word = ", word );
        i := 0;
        found := false;
        while not found do
            i := i+1;
            itword := tran[i]^(-1)*word;
            Info( InfoGroupoids, 3, "[i,itword] = ", [i,itword] );
            if ( itword = fail ) then 
                found := false;
            else
                found := itword in ch;
            fi;
        od;
        tsp := [ tran[i], itword ];
        Info( InfoGroupoids, 3, "tsp = ", tsp );
        isoe := isos[e];
        if IsMWOSinglePieceRep( Source( isoe ) ) then
            part := [ [1] ];
        else
            part := PartitionOfSource( isoe );
        fi;
        if HasPiecesOfMapping( isoe ) then
            cw := PieceOfObject( u, itword![2] );
            pw := Position( Pieces( u ), cw ); 
            fw := List( part, p -> Position( p, pw ) );
            iw := PositionProperty( fw, f -> not( f = fail ) ); 
            isow := PiecesOfMapping( isoe )[iw];
        else
            isow := isoe;
        fi;
        Info( InfoGroupoids, 3, "isow = ", isow );
        im := ImageElm( isow, tsp[2] );
        Info( InfoGroupoids, 2, tsp[2], " mapped over to ", im );
        w[k2-1] := tsp[1];
        if isfp then
            w[k2+1] := NormalFormKBRWS( gpds[he], GroupoidElement( 
                gpds[he], im![2]*w[k2+1]![2], im![3], w[k2+1]![4] ) );
            Info( InfoGroupoids, 2, "k = ", k, ", w = ", w );
        else
            w[k2+1] := im*w[k2+1];
        fi;
        Info( InfoGroupoids, 2, "k = ", k, ", w = ", w );
        lenred := ( k > 1 );
        while lenred do
            ## test for a length reduction
            e := w[k2];
            ie := InvolutoryArcs( dig )[e];
            v := adig[e][2];
            gv := gpds[ Position( vdig, v ) ];
            if isfp then
                isid := ( ( Length( w[k2-1]![2] ) = 0 ) and ( w[k2-2] = ie ) );
            else
                isid := ( ( w[k2-1]![2] = ( ) ) and ( w[k2-2] = ie ) );
            fi;
            if isid then
                Info( InfoGroupoids, 2, "LENGTH REDUCTION!\n" );
                ### perform a length reduction ###
                u := adig[e][3];
                gu := gpds[ Position( vdig, u ) ];
                if isfp then
                    ng := NormalFormKBRWS( gu, w[k2-3]*w[k2+1] );
                else
                    ng := w[k2-3]*w[k2+1];
                fi;
                w := Concatenation( w{[1..k2-4]}, [ng], w{[k2+2..lw]} );
                len := len - 2;
                lw := lw - 4;
                Info( InfoGroupoids, 2, "k = ", k, ", shorter w = ", w );
                if ( len = 0 ) then
                     rw := GraphOfGroupoidsWordNC( gg, u, w );
                     SetTailOfGraphOfGroupsWord( rw, u );
                     SetHeadOfGraphOfGroupsWord( rw, u );
                     return rw;
                else
                     k := k-2;
                     k2 := k2-4;
                     lenred := ( k > 1 );
                fi;
            else
                lenred := false;
            fi;
        od;
        k := k+1;
    od;
    ## put final groupoid element in normal form
    e := w[lw-1];
    u := adig[e][3];
    gu := gpds[ Position( vdig, u ) ];
    if isfp then
        w[lw] := NormalFormKBRWS( gu, w[lw] );
    fi;
    rw := GraphOfGroupoidsWordNC( gg, tw, w );
    SetTailOfGraphOfGroupsWord( rw, tw );
    SetHeadOfGraphOfGroupsWord( rw, hw );
    SetIsReducedGraphOfGroupoidsWord( rw, true );
    return rw;
end);
 
##############################################################################
##
#M  \=( <ggw1>, <ggw2> ) . . . . test if two graph of groupoid words are equal
##
InstallOtherMethod( \=,
    "generic method for two graph of groupoids words",
    IsIdenticalObj, [ IsGraphOfGroupoidsWordRep, IsGraphOfGroupoidsWordRep ], 0,
function ( w1, w2 )
return ( ( GraphOfGroupoidsOfWord(w1) = GraphOfGroupoidsOfWord(w2) )
     and ( TailOfGraphOfGroupsWord( w1 ) = TailOfGraphOfGroupsWord( w2 ) )
     and ( WordOfGraphOfGroupoidsWord(w1) = WordOfGraphOfGroupoidsWord(w2) ) );
end );

##############################################################################
##
#M  \*( <ggw1>, <ggw2> ) . . . . . . . product of two graph of groupoids words
##
InstallOtherMethod( \*, "generic method for two graph of groupoids words",
    IsIdenticalObj, [ IsGraphOfGroupoidsWordRep, IsGraphOfGroupoidsWordRep ], 0,
function ( ggw1, ggw2 )

    local w1, w2, h1, len1, len2, w;

    if not ( HeadOfGraphOfGroupsWord(ggw1)=TailOfGraphOfGroupsWord(ggw2) ) then
        Info( InfoGroupoids, 1, 
              "Head <> Tail for GraphOfGroupsWord(ggw1), so no composite" );
        return fail;
    fi;
    w1 := WordOfGraphOfGroupoidsWord( ggw1 );
    w2 := WordOfGraphOfGroupoidsWord( ggw2 );
    len1 := Length( w1 );
    len2 := Length( w2 );
    w := Concatenation( w1{[1..len1-1]}, [w1[len1]*w2[1]], w2{[2..len2]} );
    return GraphOfGroupoidsWord( GraphOfGroupoidsOfWord(ggw1), 
                                 TailOfGraphOfGroupsWord(ggw1), w );
end );

##############################################################################
##
#M  InverseOp( <ggword> ) . . . . . . . . inverse of a graph of groupoids word
##
InstallOtherMethod( InverseOp, "generic method for a graph of groupoids word",
    true, [ IsGraphOfGroupoidsWordRep ], 0,
function ( ggw ) 

    local gg, ie, i, j, w, len, iw, iggw;

    w := WordOfGraphOfGroupoidsWord( ggw );
    gg := GraphOfGroupoidsOfWord( ggw );
    ie := InvolutoryArcs( DigraphOfGraphOfGroupoids( gg ) );
    len := Length( w );
    iw := ShallowCopy( w );
    i := 1;
    j := len;
    iw[1] := w[len]^-1;
    while ( i < len ) do
        iw[i+1] := ie[ w[j-1] ];
        i := i+2;
        j := j-2;
        iw[i] := w[j]^(-1);
    od;
    iggw := GraphOfGroupoidsWord( gg, HeadOfGraphOfGroupsWord( ggw ), iw );
    SetHeadOfGraphOfGroupsWord( iggw, TailOfGraphOfGroupsWord( ggw ) );
    if IsReducedGraphOfGroupoidsWord( ggw ) then
        iggw := ReducedGraphOfGroupoidsWord( iggw );
    fi;
    return iggw;
end );

##############################################################################
##
#M  \^( <ggw>, <n> ) . . . . . . . . . . .  power of a graph of groupoids word
##
InstallOtherMethod( \^,
    "generic method for n-th power of a graph of groupoids word",
    true, [ IsGraphOfGroupoidsWordRep, IsInt ], 0,
function ( ggw, n ) 

    local w, tv, gg, g, k, iggw, ggwn;

    if ( n = 1 ) then
        return ggw;
    elif ( n = -1 ) then
        return InverseOp( ggw );
    fi;
    if not ( HeadOfGraphOfGroupsWord(ggw) = TailOfGraphOfGroupsWord(ggw) ) then
        Info( InfoGroupoids, 1, 
              "Head <> Tailfor GraphOfGroupsWord(ggw), so no composite" );
        return fail;
    fi;
    if ( n = 0 ) then
        tv := TailOfGraphOfGroupsWord( ggw );
        gg := GraphOfGroupoidsOfWord( ggw );
        g := GroupoidsOfGraphOfGroupoids( gg )[tv];
        return GraphOfGroupoidsWordNC( gg, tv, [ One(g) ] ); 
    elif ( n > 1 ) then
        ggwn := ggw;
        for k in [2..n] do
            ggwn := ggwn * ggw;
        od;
    elif ( n < -1 ) then
        iggw := InverseOp( ggw );
        ggwn := iggw;
        for k in [2..-n] do
            ggwn := ggwn * iggw;
        od;
    fi;
    return ggwn;
end );
