from fastapi import FastAPI, UploadFile, File
from pydantic import BaseModel
from typing import List, Optional, Union, Dict
from pyswip import Prolog
import re

app = FastAPI()

# -------------------------
# Global Prolog engine
# -------------------------
prolog = Prolog()
prolog.consult("likeness_scoring.pl")  # consult once at startup

# -------------------------
# Pydantic models
# -------------------------
class ResumeTerm(BaseModel):
    term: str
    start: int
    end: int

class JobTerm(BaseModel):
    term: str
    importance: float
    start: Optional[Union[int, str]] = "none"
    end: Optional[Union[int, str]] = "none"

class ScoreRequest(BaseModel):
    resume: List[ResumeTerm]
    job: List[JobTerm]

# -------------------------
# Helpers
# -------------------------
def clear_terms():
    """
    Completely clear resume_term/3 and job_term/4 from the Prolog KB.
    This is called before every scoring run.
    """
    list(prolog.query("retractall(resume_term(_,_,_))"))
    list(prolog.query("retractall(job_term(_,_,_,_))"))

def set_resume_terms(terms: List[ResumeTerm]):
    for t in terms:
        prolog.assertz(f"resume_term('{t.term.lower()}', {t.start}, {t.end})")

def set_job_terms(terms: List[JobTerm]):
    for t in terms:
        s = t.start if t.start != "none" else "none"
        e = t.end if t.end != "none" else "none"
        prolog.assertz(f"job_term('{t.term.lower()}', {t.importance}, {s}, {e})")

def merge_job_terms(raw_terms: List[Dict]) -> List[Dict]:
    """
    Defensive merge: ensure there is only one job term per skill.
    """
    term_map: Dict[str, Dict] = {}

    for t in raw_terms:
        term = t.get("term")
        if term is None:
            continue

        importance = t.get("importance")
        start = t.get("start", "none")
        end = t.get("end", "none")

        if term not in term_map:
            term_map[term] = {
                "term": term,
                "importance": None,
                "start": "none",
                "end": "none",
            }

        entry = term_map[term]

        # Handle importance
        if importance is not None:
            try:
                imp_val = float(importance)
            except (ValueError, TypeError):
                imp_val = None

            if imp_val is not None and 0.0 <= imp_val <= 1.0:
                entry["importance"] = imp_val

        # Explicit start/end
        if start is not None and start != "none":
            entry["start"] = start
        if end is not None and end != "none":
            entry["end"] = end

    for entry in term_map.values():
        if entry["importance"] is None:
            entry["importance"] = 1.0

    return list(term_map.values())

# -------------------------
# Parsing endpoints
# -------------------------
@app.post("/parse/resume")
async def parse_resume(file: UploadFile = File(...)):
    text = (await file.read()).decode("utf-8")
    experience = []
    for line in text.splitlines():
        m = re.match(r"([\w\.#]+),\s*(\d{6}),\s*(\d{6})", line)
        if m:
            term, start, end = m.groups()
            experience.append({
                "term": term.lower(),
                "start": int(start),
                "end": int(end)
            })
    return {"resume_terms": experience, "status": "Ready for validation"}

@app.post("/parse/job")
async def parse_job(file: UploadFile = File(...)):
    text = (await file.read()).decode("utf-8")
    job_terms = []
    for line in text.splitlines():
        m = re.match(r"([\w\.#]+),\s*([\d\.]+)", line)
        if m:
            term, imp = m.groups()
            job_terms.append({
                "term": term.lower(),
                "importance": float(imp),
                "start": "none",
                "end": "none",
            })
    return {"job_terms": job_terms, "status": "Ready for validation"}

# -------------------------
# Final scoring
# -------------------------
@app.post("/score/from-text")
def score_from_text(payload: ScoreRequest):
    # 1) Clean KB
    clear_terms()

    # 2) Merge job terms on backend, just in case
    job_terms_dicts = [jt.model_dump() for jt in payload.job]
    merged_jobs = merge_job_terms(job_terms_dicts)
    merged_job_models = [JobTerm(**j) for j in merged_jobs]

    # 3) Assert fresh facts
    set_resume_terms(payload.resume)
    set_job_terms(merged_job_models)

    # Optional: debug what Prolog thinks it has
    # job_facts = list(prolog.query("findall(T-I, job_term(T,I,_,_), L)"))
    # print("JOB FACTS:", job_facts)

    # 4) Run score
    result = list(prolog.query("final_score(Score, Breakdown)"))
    if not result:
        return {"error": "No score returned"}

    r = result[0]
    breakdown = r["Breakdown"]

    breakdown_strs = [str(entry) for entry in breakdown]

    return {
        "score": float(r["Score"]),
        "breakdown": breakdown_strs,
    }
