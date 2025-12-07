from fastapi import FastAPI
from pydantic import BaseModel
from pyswip import Prolog
from typing import List, Optional, Union

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
    start: int   # YYYYMM
    end: int     # YYYYMM

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
        prolog.assertz(
            f"resume_term('{t.term.lower()}', {t.start}, {t.end})"
        )

def set_job_terms(terms):
    prolog.query("retractall(job_term(_,_,_,_))")
    for t in terms:
        s = t.start if t.start != "none" else "none"
        e = t.end if t.end != "none" else "none"
        prolog.assertz(
            f"job_term('{t.term.lower()}', {t.importance}, {s}, {e})"
        )


# -------------------------
# API Endpoint
# -------------------------
@app.post("/score")
def get_dynamic_score(payload: ScoreRequest):

    # Inject new facts into Prolog
    set_resume_terms(payload.resume)
    set_job_terms(payload.job)

    # Run scoring engine
    result = list(prolog.query("final_score(Score, Breakdown)"))

    if not result:
        return {"error": "No score returned"}

    r = result[0]

    return {
        "score": float(r["Score"]),
        "breakdown": str(r["Breakdown"])
    }
