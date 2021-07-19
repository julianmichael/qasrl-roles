# qasrl-roles
This is the repository for the paper *Inducing Semantic Roles Without Syntax*, by Julian Michael and
Luke Zettlemoyer, published in *Findings of ACL* 2021.

> **Abstract:** Semantic roles are a key component of linguistic predicate-argument structure, but
> developing ontologies of these roles requires significant expertise and manual effort.
> Methods exist for automatically inducing semantic roles using syntactic representations, but
> syntax can also be difficult to define, annotate, and predict.
> We show it is possible to automatically induce semantic roles from
> QA-SRL, a scalable and ontology-free semantic annotation scheme that uses question-answer pairs to
> represent predicate-argument structure.
> By associating arguments with distributions over QA-SRL questions and clustering them in a
> mixture model, our method outperforms all previous models as well as a new state-of-the-art
> baseline over gold syntax. 
> We show that our method works because QA-SRL acts as *surrogate syntax*,
> capturing non-overt arguments and syntactic alternations,
> which are central motivators for the use of semantic role labeling systems.

This repository contains code to replicate the results in the paper and supporting algorithms for
doing similar experiments with other data and features.

## Contents

The bulk of the code is written in Scala, organlized into three modules under
[`qasrl-roles/`](qasrl-roles/):
* [`clustering/`](clustering/): Implementation of hybrid flat/agglomerative clustering algorithms.
* [`modeling/`](modeling/): Construction of features and experimental/analysis pipelines.
* [`browse/`](browse/): A webapp to browse the completed clusters.

The neural network models of QA-SRL are written with PyTorch/AllenNLP, and they are available in the
[qasrl-modeling](https://github.com/julianmichael/qasrl-modeling) repository, brought in here as a
[submodule](lib/qasrl-modeling).

Manual analysis data from Section 6 of the paper is recorded in
[this Google Sheet](https://docs.google.com/spreadsheets/d/1S6CQzj5XnjZXFJg6bQZMCfLAooEc-ubfK7nqMkJ3BeI/edit).

## Usage

To run the Scala code, you need the
[Mill](https://com-lihaoyi.github.io/mill/mill/Intro_to_Mill.html)
build tool.

Then to replicate the results in the paper, run [`scripts/replicate.sh`](scripts/replicate.sh), or
just read it for guidance if you are interested in a subset of the experiments.
The main entry point is [`scripts/roles.sh`](scripts/roles.sh), which calls into
[RoleInductionApp](qasrl-roles/modeling/src-jvm/RoleInductionApp.scala).
You'll probably want to `tail -f stderr.log` in another pane/window to see all details, as stderr is
redirected there so as not to interfere with the [freelog](https://github.com/julianmichael/freelog)
console output written to stdout.


## Roleset Browser

You can browse the induced rolesets in a webapp by running the following command:
```bash
mill -i qasrl-roles.browse.serve --data conll08-lemma --mode test --domain localhost --port 8888
```
This will visualize the models that have results reported in the paper (if you have run them; see
[`scripts/replicate.sh`](scripts/replicate.sh)). To automatically reconstruct all missing rolesets,
run the browser command with the `--all` flag added (it will take a while if you need all of them).

## More?

There's a lot more functionality in this repository for related experiments, such as clustering on
lexical (masked language modeling based) features, inducing predicate senses, and jointly inducing
semantic roles and predicate senses. The results of these experiments weren't too great, but they
might be an interesting starting or reference point for others interested in similar problems.
I haven't done much in the way of documentation, so if you're interested in exploring more, reusing
any of the code or algorithms, or building on this work, please
[get in touch](mailto:julianjohnmichael@gmail.com).

## Citation

Bibtex coming soon via the ACL Anthology.
