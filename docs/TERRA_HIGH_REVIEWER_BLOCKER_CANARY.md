# Terra High Reviewer Blocker Canary
TERRA_HIGH_REVIEWER_BLOCKER_CANARY=defect

## AC-3 Fixture Evidence

Initial correct head: d96b4629d71335056c9e998db65e419505307fdc

Exact file content on the initial correct head:

```text
# Terra High Reviewer Blocker Canary
TERRA_HIGH_REVIEWER_BLOCKER_CANARY=expected
```

Defect head under exact-head review: afe397778d7714ffcb9ad6a500f1b290dbadb37d

Exact-head review evidence for this defect head:

```diff
diff --git a/docs/TERRA_HIGH_REVIEWER_BLOCKER_CANARY.md b/docs/TERRA_HIGH_REVIEWER_BLOCKER_CANARY.md
index dcccfc8..e4c7caf 100644
--- a/docs/TERRA_HIGH_REVIEWER_BLOCKER_CANARY.md
+++ b/docs/TERRA_HIGH_REVIEWER_BLOCKER_CANARY.md
@@ -1,2 +1,2 @@
 # Terra High Reviewer Blocker Canary
-TERRA_HIGH_REVIEWER_BLOCKER_CANARY=expected
+TERRA_HIGH_REVIEWER_BLOCKER_CANARY=defect
```

This file intentionally preserves the controlled defect head as the blocker
fixture. Pending external acceptance evidence is not a repository blocker.
