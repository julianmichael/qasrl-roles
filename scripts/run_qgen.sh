#!/bin/bash

BASE=`basename $0`/..
pushd $BASE

# generate input files for question generation
./scripts/roles.sh setup --data conll08-lemma

pushd lib/qasrl-modeling

# Initialize python environment
python3 -m venv env
source env/bin/activate
pip install --upgrade pip
pip install -r requirements.txt

# Download span and span->question models
# (we need the span model for the pipeline, but won't actually use it for role induction)
./scripts/download.py ac

# generate question distributions
generate_questions() {
    python qasrl/pipelines/afirst_pipeline_sequential.py \
        --span models/span_density_softmax.tar.gz
        --span_to_question models/span_to_simplified_question.tar.gz \
        --cuda_device 0 \
        --span_min_prob 0.02 \
        --question_min_prob 0.01 \
        --question_beam_size 20 \
        --input_file ../../experiments/conll08/out/qg-inputs/$1.jsonl.gz \
        --output_file ../../experiments/conll08/input/qg/$1.jsonl.gz
}

# train is used for final results, dev for development/sanity checks.
# test is not used at the moment.
generate_question train
generate_question dev
# generate_question test

# --input_file qg-features/propbank/input/train.jsonl.gz \
    # --output_file qg-features/propbank/out/train.jsonl.gz

# --input_file qasrl-v2_1/orig/test.jsonl.gz \
    # --output_file qg-features/qasrl/test.jsonl.gz

popd

popd
