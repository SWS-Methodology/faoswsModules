#! /usr/bin/sh

WORKINGDIR=$(pwd)
BASEPATH=$1
echo basepath is $BASEPATH
TEMP=$(mktemp -dt)
echo tempfile is $TEMP
RMAIN=main.R

cd $BASEPATH

# for var in ${@:2}
#  do
#    echo "$BASEPATH/$var
# done

if ! git diff-index --quiet HEAD -- "${@:2}"; then
    echo "Error: There have been changes made to this file since the last commit.
If you want to package a release, you must have it align with a git commit"
   exit 1
fi

for file in ${@:2}
  do
    echo file is "$file" copied to $TEMP
    cp --parents "$file" $TEMP
  done

CUR=$(git rev-parse HEAD)
SHORTCUR=$(echo $CUR | cut -c -6)

cd $TEMP
echo -e "# Commit: $CUR\n$(cat $RMAIN)" > "$RMAIN"
zip -FSr "$WORKINGDIR/$BASEPATH/module_$SHORTCUR.zip" .

echo Module "$WORKINGDIR/$BASEPATH/module_$SHORTCUR.zip" built!
