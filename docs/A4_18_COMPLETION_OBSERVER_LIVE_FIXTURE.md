# A4.18 Completion Observer Live Fixture
CONTROL_VALUE: A4_18_OBSERVER_PASS

## A5.3 Accepted Blocker Repair Evidence

This fixture records the trusted local audit evidence for issue 102 without
querying GitHub, changing labels, creating comments, or modifying protected
control-plane files.

### F-1 Exact Worker Completion Event

- observer_workflow: GREEN Codex worker completion observer
- observer_workflow_run_id: 33977298264
- observer_workflow_conclusion: success
- observed_worker_workflow: GREEN Codex issue worker
- observed_worker_run_id: 33977224615
- observed_worker_conclusion: success
- trigger_source: workflow_run.completed
- trigger_worker_run_id: 33977224615
- trusted_completion_marker_count_for_worker_run_id_33977224615: 1

### F-2 Single Trusted Completion Marker

The trusted completion marker is bounded to a single HTML comment and contains
only deterministic control-plane identifiers, branch/SHA identities, timestamps,
and the idempotency key.

<!-- a4.18-completion:{"branch":"codex/issue-102-a4-18-controlled-acceptance-prove-green-completion","conclusion":"success","created_at":"2026-09-05T16:07:24Z","idempotency_key":"a4.18:cde49ce2f711d54cc5461a35d23d250d3ad5c5460450dc4373eb240d1d9738b7","issue_number":102,"pr_number":103,"pr_head_sha":"ae04e93ee7b05c011108d1c8f352f58ae542764d","repository":"RayZhang2024/ML-AMstress","run_id":33977224615,"schema_version":1,"updated_at":"2026-09-05T16:14:03Z","workflow_head_branch":"main","workflow_head_sha":"83ba4e8a0c62360449b5d7dfd608dc1c7a55e0b8"} -->

- marker_length_bytes: 598
- marker_bound: less than 1024 bytes
- marker_payload_class: deterministic control-plane audit only
- marker_excluded_fields: credentials, raw logs, prompts, stderr, local paths,
  environment variables, free-form model output
- trusted_workflow_main_execution_sha: 83ba4e8a0c62360449b5d7dfd608dc1c7a55e0b8
- independently_sourced_pr_head_sha: ae04e93ee7b05c011108d1c8f352f58ae542764d

### F-3 Replay Idempotency Evidence

- replay_input_worker_run_id: 33977224615
- replay_input_idempotency_key:
  a4.18:cde49ce2f711d54cc5461a35d23d250d3ad5c5460450dc4373eb240d1d9738b7
- replay_result: existing marker reused; no new marker created
- post_replay_trusted_completion_marker_count_for_worker_run_id_33977224615: 1

### F-4 Issue Status Preservation Evidence

- post_observation_issue_labels: status:review
- post_observation_status_label_count: 1
- worker_authored_status_label_preserved: true
- observer_status_mutation: none
- observer_label_mutation: none
- provenance: observer implementation records only the completion audit comment;
  it has no label update path and does not overwrite worker-authored issue
  status.
