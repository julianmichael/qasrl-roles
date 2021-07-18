#!/bin/bash

LD_PRELOAD=libgslcblas.so mill -i qasrl.roles.modeling.jvm.runMain qasrl.roles.modeling.FrameInductionApp "$@" 2>>extra.log
