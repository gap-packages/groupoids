#############################################################################
##
##  PackageInfo.g  file for the package groupoids 
##  Emma Moore and Chris Wensley 
## 

SetPackageInfo( rec(

PackageName := "groupoids",
Subtitle := "Calculations with finite groupoids and their homomorphisms",
Version := "1.79",
Date := "11/09/2025", # dd/mm/yyyy format
License := "GPL-2.0-or-later",

Persons := [
  rec(
    LastName      := "Moore",
    FirstNames    := "Emma J.",
    IsAuthor      := true,
    IsMaintainer  := false
  ),
  rec(
    LastName      := "Wensley",
    FirstNames    := "Chris",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "cdwensley.maths@btinternet.com",
    WWWHome       := "https://github.com/cdwensley",
    Place         := "Llanfairfechan" 
  )
],

Status := "accepted",
CommunicatedBy := "Derek Holt (Warwick)",
AcceptDate := "05/2015",

SourceRepository := rec( 
  Type := "git", 
  URL := "https://github.com/gap-packages/groupoids" ),
  IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
  PackageWWWHome  := "https://gap-packages.github.io/groupoids/",
  README_URL      := Concatenation( ~.PackageWWWHome, "README.md" ),
  PackageInfoURL  := Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),
  ArchiveURL      := Concatenation( ~.SourceRepository.URL, 
                                  "/releases/download/v", ~.Version, 
                                  "/", ~.PackageName, "-", ~.Version ), 

SupportEmail := "cdwensley1234@btinternet.com",
ArchiveFormats  := ".tar.gz",

AbstractHTML :=
"The groupoids package provides a collection of functions for computing \
with finite groupoids, graph of groups, and graphs of groupoids. \
These are based on the more basic structures of magmas with objects \
and their mappings. \
It provides functions for normal forms of elements in Free Products with \
Amalgamation and in HNN extensions. \
Up until April 2017 this package was named Gpd.",

PackageDoc := rec(
  BookName  := "groupoids",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0_mj.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Finite Groupoids and Graphs of Groups"
),

Dependencies := rec(
  GAP := ">=4.10.1",
  NeededOtherPackages := [ [ "fga", ">= 1.4.0" ],
                           [ "utils", ">= 0.76" ] ],
  SuggestedOtherPackages := [ [ "semigroups", ">= 3.1.1" ] ],
  ExternalConditions := [ ]
),

AvailabilityTest := ReturnTrue,

TestFile := "tst/testall.g", 

Keywords := [ "magma with objects", "groupoid", "graph of groups", 
              "free product with amalgamation", "HNN extension", 
              "automorphisms" ], 

BannerString := Concatenation(
    "Loading groupoids ", String( ~.Version ), 
    " (algorithms for finite groupoids)\n", 
    "by Emma Moore and Chris Wensley (https://github.com/cdwensley)\n",
    "--------------------------------------------------------------\n" ),

AutoDoc := rec(
    TitlePage := rec(
        Copyright := Concatenation(
            "© 2000-2025, Emma Moore and Chris Wensley.<P/>\n", 
            "The &groupoids; package is free software; you can redistribute ", 
            "it and/or modify it under the terms of the GNU General ", 
            "Public License as published by the Free Software Foundation; ", 
            "either version 2 of the License, or (at your option) ", 
            "any later version.\n"
            ),
        Abstract := Concatenation( 
            "The &groupoids; package provides functions for computation ",
            "with groupoids (categories with every arrow invertible) and ",
            "their morphisms; for graphs of groups, ",
            "and graphs of groupoids.\n", 
            "The most basic structure introduced is that of ", 
            "<E>magma with objects</E>; followed by ", 
            "<E>semigroup with objects</E>; ",
            "then <E>monoid with objects</E>; ", 
            "and finally <E>groupoid</E>, which is a ", 
            "<E>group with objects</E>.\n <P/>", 
            "It provides normal forms for Free Products with Amalgamation ", 
            "and for HNN-extensions when the initial groups have ", 
            "rewrite systems and the subgroups have finite index. ", 
            "<P/>", 
            "The &groupoids; package was originally implemented in 2000 ", 
            "(as <Package>GraphGpd</Package>) ", 
            "when the first author was studying for a Ph.D. in Bangor.\n <P/>", 
            "The package was then renamed <Package>Gpd</Package> ", 
            "and version 1.07 was released in July 2011, ", 
            "ready for &GAP; 4.5.\n <P/>", 
            "<Package>Gpd</Package> became an accepted &GAP; package ", 
            "in May 2015.\n <P/>", 
            "In April 2017 the package was renamed again, as ", 
            "<Package>groupoids</Package>.\n <P/>",  
            "Later versions implement many of the constructions ", 
            "described in the paper <Cite Key='AlWe' /> ", 
            "for automorphisms of groupoids.\n <P/>", 
            "Bug reports, comments, suggestions for additional features, ", 
            "and offers to implement some of these, will all be ", 
            "very welcome.\n <P/>", 
            "Please submit any issues at ", 
            "<URL>https://github.com/gap-packages/groupoids/issues/</URL> ", 
            "or send an email to the second author at ", 
            "<Email>cdwensley.maths@btinternet.com</Email>.\n <P/>" 
            ), 
        Acknowledgements := Concatenation( 
            "This documentation was prepared using the ", 
            "&GAPDoc; <Cite Key='GAPDoc'/> and ", 
            "&AutoDoc; <Cite Key='AutoDoc'/> packages.<P/>\n", 
            "The procedure used to produce new releases uses the package ", 
            "<Package>GitHubPagesForGAP</Package> ", 
            "<Cite Key='GitHubPagesForGAP' /> ", 
            "and the package <Package>ReleaseTools</Package>.<P/>" 
            ),
    ) 
),

));
