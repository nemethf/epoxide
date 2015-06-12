#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

name=epoxide
ver=$(head ${DIR}/../src/${name}-pkg.el|cut -f4 -d\")
pkg=$name-$ver

ver2=$(echo $ver|tr . " ")
sed -i -e "/^\s*($name/,+1s/[[].*/[($ver2)/" www/archive-contents

cd $DIR/..
cp -a src $pkg
find $pkg -name '*.elc' -delete
find $pkg -name '*~' -delete
tar -cf $pkg.tar $pkg
rm -rf $pkg
mv $pkg.tar www

