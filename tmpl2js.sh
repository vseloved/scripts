#! /bin/sh

IN=$(basename $1);
NAME=${IN%.*};

if [ $# -lt 2 ]
then
    OUT=$NAME.js
else
    OUT=$2
fi;

echo -n "var ${NAME}Template = '" > $OUT;
tr -d "\r\n" < $1 | sed -e "s/'/\\\'/g"  >> $OUT;
echo "';" >> $OUT;