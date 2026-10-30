# hbs2 agent workflow

Use the `ub-mode` workflow for code changes. Finish every coherent
implementation stage with a quality gate over the changed scope:

- build and run the relevant behavioral tests;
- run HLint on changed Haskell files;
- run Weeder after dependency, module-boundary, or exported-API changes;
- inspect `git diff --check` and the final diff;
- fix actionable findings before committing.

Crapper, Dryer, Mutator, and UML Viewer currently have no Haskell parser. Mark
their results as `N/A (unsupported language)`; an empty scan is not a pass.
Use them normally if a stage changes a supported language. Record each gate as
pass, fail, or N/A in the stage handoff.
