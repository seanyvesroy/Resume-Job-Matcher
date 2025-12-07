from fastapi import FastAPI
from pyswip import Prolog

app = FastAPI()

prolog = Prolog()
prolog.consult("likeness_scoring.pl")

@app.get("/score")
def get_score():
    result = list(prolog.query("final_score(Score, Breakdown)"))

    if not result:
        return {"error": "No match returned"}

    r = result[0]

    return {
        "score": float(r["Score"]),
        "breakdown": str(r["Breakdown"])  # stringify Prolog structure safely
    }
def set_resume_terms(terms):
    prolog.query("retractall(resume_term(_,_,_))")
    for term, start, end in terms:
        prolog.assertz(f"resume_term('{term}', {start}, {end})")

def set_job_terms(terms):
    prolog.query("retractall(job_term(_,_,_,_))")
    for term, importance, start, end in terms:
        s = start if start != "none" else "none"
        e = end if end != "none" else "none"
        prolog.assertz(f"job_term('{term}', {importance}, {s}, {e})")
