#!/bin/bash

UPDATE=2

# Enter the src directory
BASEDIR=$(dirname $0)
cd $BASEDIR/src

./racpete.rkt 2> error.log

while [ $? -eq 2 ]
do
    git pull origin master
    ./racpete.rkt 2> error.log
done
