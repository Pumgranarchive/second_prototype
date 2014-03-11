#!/bin/sh

# export database collection
my_export(){
    mongoexport -d $1 -c $2 -o $1/$2.json;
    mongoexport -d $1 -c $2 -o $1/$2.cdb.json --jsonArray;
    echo "{\"docs\":`cat $1/$2.cdb.json`}" > $1/$2.cdb.json;
}

database=pumgrana

my_export $database contents
my_export $database tags
my_export $database links