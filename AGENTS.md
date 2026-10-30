# hbs2 agent workflow

Use the `ub-mode` workflow for code changes. Finish every coherent
implementation stage with a quality gate over the changed scope:

- build and run the relevant behavioral tests;
- run HLint on changed Haskell files;
- run Weeder after dependency, module-boundary, or exported-API changes;
- inspect `git diff --check` and the final diff;
- fix actionable findings before committing.

Crapper, Dryer, Mutator, and UML Viewer support Haskell. Run them on the
changed production modules after each stage; give Mutator the focused test
command and use `:lang :haskell` in UML policies. An empty scan is not a pass.
Record each native and companion gate as pass, fail, or N/A in the handoff.
