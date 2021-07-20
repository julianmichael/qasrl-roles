#!/bin/bash

LD_PRELOAD=libgslcblas.so mill -i qasrl-roles.modeling.jvm.runMain qasrl.roles.modeling.RoleInductionApp "$@" 2>>stderr.log
