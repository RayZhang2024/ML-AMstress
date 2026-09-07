import unittest
from scripts import yellow_lane_policy as p
from tests.test_issue28_worker import GREEN_BODY as BODY
SHA='a'*40
class PolicyTests(unittest.TestCase):
 def test_route(self):
  base={"event":"fresh-agent-codex","labels":["status:ready","risk:green"],"dependencies":True,"effective_risk":"green","scientific":False,"contract":BODY}
  self.assertEqual(p.route(base),p.GREEN); base.update(labels=["status:ready","risk:yellow"],effective_risk="yellow",contract=BODY.replace('risk:green','risk:yellow')); self.assertEqual(p.route(base),p.YELLOW)
  for key,value in (("labels",["status:ready","risk:red"]),("event","old"),("scientific",True)): base[key]=value; self.assertEqual(p.route(base),p.REJECT)
 def test_branch_evidence_claim_replay(self):
  b=p.yellow_branch(7,'Hi, 世界!!'); self.assertEqual(b,'codex-yellow/issue-7-hi'); self.assertRaises(p.PolicyError,p.yellow_branch,0,'x')
  e={"schema_version":1,"repository":p.REPOSITORY,"issue_number":7,"trusted_base_sha":SHA,"declared_risk":"yellow","effective_risk":"yellow","authorized_paths":["docs/x.md"],"scientific_runtime_prohibited":True}; s=p.prestart(e,SHA); self.assertEqual(p.prestart(e,SHA),s)
  c={"schema_version":1,"repository":p.REPOSITORY,"issue_number":7,"trusted_base_sha":SHA,"branch":b,"lane":"yellow"}; q=p.claim(c,SHA); self.assertEqual(p.replay([q],q),'idempotent'); self.assertRaises(p.PolicyError,p.replay,[q],q+'x')
  e['authorized_paths']=['../bad']; self.assertRaises(p.PolicyError,p.prestart,e,SHA)
