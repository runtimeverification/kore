#!/bin/bash

set -eux

TIMESTAMP=$(date +"%F-%H-%M")

TARGET=${1:-"hs-backend-merge-${TIMESTAMP}"}

which git-filter-repo || (echo "git-filter-repo required"; exit 2)

if [ -f ${TARGET} ]; then
    echo "Target directory $TARGET exists, aborting script"
    exit 1
fi

# use absolute paths for all subsequent commands
TARGET=$(realpath ${TARGET})

ASSETS="add-booster-project-config.patch add-booster-gitignore.patch"

SCRIPTDIR=$(dirname $0)

pushd $SCRIPTDIR

for f in $ASSETS; do
    if [ ! -f "./$f" ]; then
        echo "Missing asset file $f, aborting script"
        exit 1
    fi
done

git clone git@github.com:runtimeverification/haskell-backend.git $TARGET

git clone git@github.com:runtimeverification/hs-backend-booster.git ${TARGET}-booster

pushd ${TARGET}-booster
time git-filter-repo --to-subdirectory-filter booster
popd

cp -t $TARGET $ASSETS

pushd $TARGET
# merge repositories
git checkout -b booster-merge-$TIMESTAMP
git remote add booster ${TARGET}-booster
git fetch booster --no-tags
git merge booster/main --allow-unrelated-histories --no-edit
git remote remove booster

#patch result
git mv booster/dev-tools ./dev-tools
git commit -m "Move dev-tools package to top"
git rm booster/stack.yaml booster/stack.yaml.lock booster/cabal.project booster/.gitignore
patch < add-booster-project-config.patch
patch < add-booster-gitignore.patch
stack ls dependencies
git commit -a -m "Add booster project configuration, remove stale booster files"
popd

popd
