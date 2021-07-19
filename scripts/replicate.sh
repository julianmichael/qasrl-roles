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

run_lemma () { ./scripts/roles.sh run --data conll08-lemma --mode test --model "$@" }
run_sense () { ./scripts/roles.sh run --data conll08-sense --mode test --model "$@" }

# Main experiments (Table 3)
run_lemma "arg/syntf"          # syntf
run_lemma "arg/mnd->syntf"     # syntf +lex
run_lemma "arg/syntf+"         # syntf +pass->act
run_lemma "arg/mnd->syntf+"    # syntf +all rules
run_lemma "arg/qent+dv"        # HUM-QQ
run_lemma "arg/qent"           # HUM-QQ -cp
run_lemma "arg/mnd->qent+dv"   # HUM-QQ +lex

# Ablation/analysis experiments (Table 6)
run_lemma "arg/mnd->qent_argadj+dv" # argument/adjunct oracle
run_sense "arg/mnd->qent+dv"        # predicate sense oracle
run_sense "arg/mnd->qent_argadj+dv" # combined oracle

# Extra experiments used in appendix

# Table 7
run_lemma "arg/n->syntf" # negation rule only
run_lemma "arg/m->syntf" # modal rule only
run_lemma "arg/d->syntf" # discourse rule only
# Table 8
run_lemma "arg/mnd->qent" --tune entropy --tune num-clusters --tune oracle
# Table 10
run_lemma "arg/mnd->qent+pent" # HUM-QQ +lex +MI


# ./scripts/roles.sh run --data conll08-lemma --mode train --model arg/syntf        # syntf
# ./scripts/roles.sh run --data conll08-lemma --mode train --model arg/mnd->syntf   # syntf +lex
# ./scripts/roles.sh run --data conll08-lemma --mode train --model arg/syntf+       # syntf +pass->act
# ./scripts/roles.sh run --data conll08-lemma --mode train --model arg/mnd->syntf+  # syntf +all rules
# ./scripts/roles.sh run --data conll08-lemma --mode train --model arg/qent+dv      # HUM-QQ
# ./scripts/roles.sh run --data conll08-lemma --mode train --model arg/qent         # HUM-QQ -cp
# ./scripts/roles.sh run --data conll08-lemma --mode train --model arg/mnd->qent    # HUM-QQ +lex

popd

popd
