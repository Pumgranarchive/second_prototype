#!/bin/sh

database=pumgrana

# ### CouchDB
# curl -X DELETE http://127.0.0.1:5984/$database
# curl -X PUT http://127.0.0.1:5984/$database
# 
# #couchdb_import database collection
# couchdb_import(){
#     curl -X PUT http://127.0.0.1:5984/$1/$2
#     curl -X PUT -d @$1/$2.cdb.json http://127.0.0.1:5984/$1/$2
# }
# 
# couchdb_import $database contents
# ouchdb_import $database tags
# couchdb_import $database links

### MongoDB
mongo $database --eval "db.dropDatabase()"

#mongodb_import database collection
mongodb_import(){
    mongoimport --db $1 --collection $2 --type json --file $1/$2.json
}

mongodb_import $database contents
mongodb_import $database tags
mongodb_import $database links
