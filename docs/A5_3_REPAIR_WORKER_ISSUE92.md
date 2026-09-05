# A5.3 Issue #92 compatibility note

The trusted A5.3 repair subprocess now explicitly selects `gpt-5.5` for the repository's pinned `codex-cli 0.149.1` runner contract instead of inheriting the ChatGPT-account default model.

The repair boundary otherwise remains unchanged: prompt via stdin, `workspace-write` sandbox, approval policy `never`, no shell, GitHub/OpenAI/App token stripping, exact-head/branch checks, trusted allowed-path enforcement, trusted validation, commit creation, and race-safe push.

Expected `RepairError` failures may add only an explicitly allowlisted static detail to the bounded trusted repair-failure audit marker. Arbitrary exception text, raw Codex stdout/stderr, prompts, credentials, environment values, and local paths remain unpublished.
