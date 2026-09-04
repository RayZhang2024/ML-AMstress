# A5.3 bounded same-branch repair worker

`scripts/a5_repair_worker.py` is a local, trusted execution boundary for one
already accepted A5.1 blocker verdict. It has no GitHub API client and does not
call the reviewer or A5.2 state engine. A future trusted orchestrator supplies
an immutable `RepairRequest`, and remains responsible for all GitHub state
transitions.

The request is schema version 1 and must name one exact existing PR branch and
40-character expected head, the accepted A5.2 blocker decision key, the exact
`status:in-progress`/`review:blocker` state, matching review evidence head,
GREEN effective risk, accepted non-scientific findings, an exact path allowlist,
and attempt 1 or 2. Protected control-plane paths are always denied.

Before Codex runs, the worker checks a clean worktree, exact branch, and exact
local head. Codex receives a bounded prompt on stdin, runs only with
workspace-write and approval disabled, and has GitHub/OpenAI API credentials
removed along with inherited Git configuration. Model output is never evidence.

After execution, Git determines all changed paths. Only explicit modify/create
changes inside the allowlist are accepted. Syntax checks, the normal-Python
unit suite, and `git diff --check` run using trusted argv commands. On success,
one deterministic commit is made and pushed only to the existing branch using
an expected-head `--force-with-lease`; a moved remote head fails closed.

The returned `RepairResult` contains only bounded audit identity, finding IDs,
changed paths, validation status, heads, and a deterministic A5.3 decision key.
It excludes prompts, model streams, credentials, and diff content.
