# oracle-drawio-erd

Automatically create database ERD diagram from existing oracle tables using pl/sql function.

The latest drawio release can be found there: https://github.com/jgraph/drawio-desktop/releases/

# usage
1. Set function parameter as example:
include_objects_like => 'SOME_TABLE%'

2. Copy result to clipboard

3. In drawio, menu -> extras -> edit diagram  -> {paste result here} -> select "Add to existing drawing" in drodpdown list and presss "Ok"
