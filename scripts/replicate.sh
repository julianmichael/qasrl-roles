#!/bin/bash

BASE=`basename $0`
pushd $BASE/..

git submodule init
git submodule update

# publish local branch of evilplot (I had to update the scalajs version)
pushd lib/evilplot
sbt publishLocal
popd

# Ensure LDC artifacts are downloaded and extracted in data/
python3 scripts/check_ldc.py

# Download the predicted question distributions.
# To produce these yourself, see scripts/run_qgen.sh
mkdir -p experiments/conll08/input/qg
pushd experiments/conll08/input/qg
curl https://www.dropbox.com/sh/tzok99c9wxq24c1/AADtnarghu5shMnIWjh5yMQwa?dl=1 -L -O -J
unzip qg.zip
rm qg.zip
popd

pushd ..

run_train () {
    ./scripts/roles.sh run --data conll08-lemma --mode train --model $1
}

# main experiments
run_train arg/syntf        # syntf
run_train arg/mnd->syntf   # syntf +lex
run_train arg/syntf+       # syntf +pass->act
run_train arg/mnd->syntf+  # syntf +all rules
run_train arg/qent+dv      # HUM-QQ
run_train arg/qent         # HUM-QQ -cp
run_train arg/mnd->qent    # HUM-QQ +lex

# ./scripts/roles.sh run --data conll08-lemma --mode train --model arg/syntf        # syntf
# ./scripts/roles.sh run --data conll08-lemma --mode train --model arg/mnd->syntf   # syntf +lex
# ./scripts/roles.sh run --data conll08-lemma --mode train --model arg/syntf+       # syntf +pass->act
# ./scripts/roles.sh run --data conll08-lemma --mode train --model arg/mnd->syntf+  # syntf +all rules
# ./scripts/roles.sh run --data conll08-lemma --mode train --model arg/qent+dv      # HUM-QQ
# ./scripts/roles.sh run --data conll08-lemma --mode train --model arg/qent         # HUM-QQ -cp
# ./scripts/roles.sh run --data conll08-lemma --mode train --model arg/mnd->qent    # HUM-QQ +lex

popd

popd
