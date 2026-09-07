"""Pure, non-activating policy primitives for a future trusted YELLOW lane."""
from __future__ import annotations
import hashlib, json, re
from dataclasses import dataclass
from typing import Any, Mapping, Sequence
from scripts import codex_issue_worker as green

REPOSITORY = green.REPOSITORY
SHA_RE = re.compile(r"^[0-9a-f]{40}$")
BRANCH_RE = re.compile(r"^codex-yellow/issue-([1-9][0-9]*)-([a-z0-9]+(?:-[a-z0-9]+)*)$")
MAX_PATHS, MAX_PATH, MAX_SLUG = 32, 240, 50
GREEN, YELLOW, REJECT = "GREEN", "YELLOW", "REJECT"

class PolicyError(ValueError): pass

def _positive(value: Any) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value < 1: raise PolicyError("issue number")
    return value
def _sha(value: Any) -> str:
    if not isinstance(value, str) or not SHA_RE.fullmatch(value): raise PolicyError("trusted base SHA")
    return value
def _path(value: Any) -> str:
    if not isinstance(value, str) or not value or len(value)>MAX_PATH: raise PolicyError("authorized path")
    if value.startswith(("/", "\\")) or re.match(r"^[A-Za-z]:",value) or "\\" in value or "*" in value or any(x in ("", ".", "..") for x in value.split("/")): raise PolicyError("authorized path")
    return value
def yellow_branch(issue_number: int, title: str) -> str:
    _positive(issue_number)
    if not isinstance(title,str): raise PolicyError("title")
    slug=re.sub(r"[^a-z0-9]+","-",title.lower().encode("ascii","ignore").decode()).strip("-")[:MAX_SLUG].rstrip("-") or "yellow-work"
    branch="codex-yellow/issue-%d-%s"%(issue_number,slug)
    validate_yellow_branch(branch,issue_number); return branch
def validate_yellow_branch(branch: Any, issue_number: int|None=None) -> str:
    if not isinstance(branch,str) or len(branch)>200 or any(ord(c)<32 for c in branch): raise PolicyError("yellow branch")
    match=BRANCH_RE.fullmatch(branch)
    if not match or (issue_number is not None and int(match.group(1))!=_positive(issue_number)): raise PolicyError("yellow branch")
    return branch
def route(snapshot: Mapping[str,Any]) -> str:
    try:
        if not isinstance(snapshot,Mapping) or snapshot.get("event")!="fresh-agent-codex": raise PolicyError("event")
        labels=tuple(snapshot.get("labels",()))
        risks=[x for x in labels if isinstance(x,str) and x.startswith("risk:")]; statuses=[x for x in labels if isinstance(x,str) and x.startswith("status:")]
        if statuses != ["status:ready"] or len(risks)!=1 or snapshot.get("dependencies") is not True or snapshot.get("effective_risk") not in ("green","yellow") or snapshot.get("scientific") is not False: raise PolicyError("eligibility")
        contract=green.parse_contract(snapshot.get("contract",""),REPOSITORY)
        return GREEN if risks[0]=="risk:green" and contract.risk=="risk:green" and snapshot["effective_risk"]=="green" else YELLOW if risks[0]=="risk:yellow" and contract.risk=="risk:yellow" and snapshot["effective_risk"]=="yellow" else REJECT
    except Exception: return REJECT
def _canonical(payload: Mapping[str,Any], keys: Sequence[str]) -> str:
    if not isinstance(payload,Mapping) or set(payload)!=set(keys): raise PolicyError("schema")
    return json.dumps(dict(payload),sort_keys=True,separators=(",",":"))
def prestart(payload: Mapping[str,Any], expected_base: str|None=None) -> str:
    keys=("schema_version","repository","issue_number","trusted_base_sha","declared_risk","effective_risk","authorized_paths","scientific_runtime_prohibited")
    _canonical(payload,keys)
    if payload["schema_version"]!=1 or payload["repository"]!=REPOSITORY or _positive(payload["issue_number"]) is None or _sha(payload["trusted_base_sha"])!=(expected_base or payload["trusted_base_sha"]) or payload["declared_risk"]!="yellow" or payload["effective_risk"]!="yellow" or payload["scientific_runtime_prohibited"] is not True: raise PolicyError("prestart")
    paths=payload["authorized_paths"]
    if not isinstance(paths,list) or not 1<=len(paths)<=MAX_PATHS or len(set(paths))!=len(paths): raise PolicyError("paths")
    for path in paths:_path(path)
    return _canonical(payload,keys)
def claim(payload: Mapping[str,Any], expected_base: str|None=None) -> str:
    keys=("schema_version","repository","issue_number","trusted_base_sha","branch","lane")
    _canonical(payload,keys)
    if payload["schema_version"]!=1 or payload["repository"]!=REPOSITORY or payload["lane"]!="yellow" or _sha(payload["trusted_base_sha"])!=(expected_base or payload["trusted_base_sha"]): raise PolicyError("claim")
    validate_yellow_branch(payload["branch"],payload["issue_number"]); return _canonical(payload,keys)
def replay(records: Sequence[str], candidate: str) -> str:
    if len(records)>1: raise PolicyError("ambiguous evidence")
    return "new" if not records else "idempotent" if records[0]==candidate else (_ for _ in ()).throw(PolicyError("conflicting evidence"))
