# A5.3 deterministic repair commit identity (Issue #94)

A5.3 runs Git subprocesses with system and global Git configuration disabled. The live A5.4b fixture proved that repair execution could reach the trusted commit stage but then failed with the bounded audit detail `could not create repair commit`.

Issue #94 keeps that isolation boundary and supplies a deterministic automation-owned identity only to the trusted `git commit` operation. The commit command uses process-local `-c` configuration for `user.useConfigOnly=true`, `user.name`, and `user.email`; inherited `GIT_AUTHOR_*` and `GIT_COMMITTER_*` variables are removed from the isolated subprocess environment.

This does not give Codex commit or push authority. Codex still runs separately with the existing token-stripped environment and is required not to commit. The trusted A5.3 worker remains responsible for validation, staging, exactly one repair commit, ancestry checks, and race-safe App-token push.

`MAX_REPAIR_ATTEMPTS` remains 2. PR #83 is not reset or reused after its two exhausted attempts; parent #76 will use a fresh docs-only fixture after this protected fix is merged.
