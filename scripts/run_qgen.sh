#!/bin/bash

python qfirst/pipelines/afirst_pipeline_sequential.py \
       --span ../qasrl-models/bert-feature/span/density_softmax/7.json/current \
       --span_to_question ../qasrl-models/bert-feature/span_to_question/no_tan_or_anim/2.json/current \
       --cuda_device 0 \
       --span_min_prob 0.02 \
       --question_min_prob 0.01 \
       --question_beam_size 20 \
       --input_file experiments/conll08/out/qg-inputs/test.jsonl.gz \
       --output_file experiments/conll08/input/qg/test.jsonl.gz

# --input_file qg-features/propbank/input/train.jsonl.gz \
    # --output_file qg-features/propbank/out/train.jsonl.gz

# --input_file qasrl-v2_1/orig/test.jsonl.gz \
    # --output_file qg-features/qasrl/test.jsonl.gz
