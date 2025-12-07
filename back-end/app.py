from fastapi import FastAPI, UploadFile, File
from pydantic import BaseModel
from pyswip import Prolog
from typing import List, Optional, Union
import re

app = FastAPI()

# -------------------------
# Prolog Engine
# -------------------------
prolog = Prolog()
prolog.consult("likeness_scoring.pl")

# -------------------------
# Data Models
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
# Prolog Fact Injection
# -------------------------
def set_resume_terms(terms):
    prolog.query("retractall(resume_term(_,_,_))")
    for t in terms:
        prolog.assertz(f"resume_term('{t.term.lower()}', {t.start}, {t.end})")

def set_job_terms(terms):
    prolog.query("retractall(job_term(_,_,_,_))")
    for t in terms:
        s = t.start if t.start != "none" else "none"
        e = t.end if t.end != "none" else "none"
        prolog.assertz(f"job_term('{t.term.lower()}', {t.importance}, {s}, {e})")

# -------------------------
# RESUME PARSER
# -------------------------
@app.post("/parse/resume")
async def parse_resume(file: UploadFile = File(...)):
    text = (await file.read()).decode("utf-8")

    experience = []
    lines = text.splitlines()

    for line in lines:
        m = re.match(r"([\w\.#]+),\s*(\d{6}),\s*(\d{6})", line)
        if m:
            term, start, end = m.groups()
            experience.append({
                "term": term.lower(),
                "start": int(start),
                "end": int(end)
            })

    return {
        "resume_terms": experience,
        "status": "Ready for validation"
    }

# -------------------------
# JOB PARSER
# -------------------------
@app.post("/parse/job")
async def parse_job(file: UploadFile = File(...)):
    text = (await file.read()).decode("utf-8")

    job_terms = []
    lines = text.splitlines()

    for line in lines:
        m = re.match(r"([\w\.#]+),\s*([\d\.]+)", line)
        if m:
            term, imp = m.groups()
            job_terms.append({
                "term": term.lower(),
                "importance": float(imp),
                "start": "none",
                "end": "none"
            })

    return {
        "job_terms": job_terms,
        "status": "Ready for validation"
    }

# -------------------------
# FINAL SCORING ENDPOINT
# -------------------------
@app.post("/score/from-text")
def score_from_text(payload: ScoreRequest):

    set_resume_terms(payload.resume)
    set_job_terms(payload.job)

    result = list(prolog.query("final_score(Score, Breakdown)"))

    if not result:
        return {"error": "No score returned"}

    r = result[0]

    return {
        "score": float(r["Score"]),
        "breakdown": str(r["Breakdown"])
    }
