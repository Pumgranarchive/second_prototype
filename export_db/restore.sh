#!/bin/sh

mongo pumgrana --eval "db.dropDatabase()"
mongorestore pumgrana