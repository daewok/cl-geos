# CL-GEOS #

CL-GEOS is a Common Lisp library for performing geometric operations. It allows
you to create geometric objects (points, lines, rings, polygons, etc.) and
easily query for intersections, area, centroid, and so on. Most of the heavy
lifting is done by the excellent [GEOS](http://trac.osgeo.org/geos/) library.

Most of the GEOS API for I/O, predicates, and topology are implemented, however
this library is still in beta. Every effort will be made to preserve the
external API when bumping up to version 1.0.

# Prerequisites #

Requires the GEOS library. On Debian based systems, this can be accomplished by
running:

    apt-get install libgeos-dev

# Gotchas #

The geos package exports `INTERSECTION` and `UNION` which are two functions
defined by CL. You'll want to shadow import these if you want the GEOS versions
instead of the CL ones.
