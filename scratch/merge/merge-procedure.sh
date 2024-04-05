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

ASSETS="add-booster-project-config.patch add-booster-gitignore.patch tweak-nix-flake.patch"

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

# build setup for all packages
git mv booster/dev-tools ./dev-tools
git commit -m "Move dev-tools package to top"
git rm booster/stack.yaml booster/stack.yaml.lock booster/cabal.project booster/.gitignore
patch < add-booster-project-config.patch
patch < add-booster-gitignore.patch
stack ls dependencies
git commit -a -m "Add booster project configuration, remove stale booster files"

# cabal freeze file
git mv -f booster/scripts/freeze-cabal-to-stack-resolver.sh scripts/
git rm booster/cabal.project.freeze
scripts/freeze-cabal-to-stack-resolver.sh
git add ./cabal.project.freeze
git commit -a -m "update cabal freeze file and generating script"

# make flake
git rm booster/flake.nix booster/flake.lock
patch < tweak-nix-flake.patch
nix flake lock
git commit -a -m "flake.nix: add booster artefacts and modify setup, remove booster flake"

# adapt fourmolu and hlint scripts, run them once to check
git mv -f booster/scripts/fourmolu.sh scripts/fourmolu.sh
git rm booster/fourmolu.yaml
scripts/fourmolu.sh -c
git commit -a -m "Adapt fourmolu setup + reformat two booster files"
git mv booster/scripts/hlint.sh scripts/hlint.sh
patch < adapt-hlint.patch
scripts/hlint.sh
git commit -a -m "Adapt hlint setup"


popd

popd
